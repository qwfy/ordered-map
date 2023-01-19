#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::iter::DoubleEndedIterator;
use std::ops::Index;
use std::vec::Vec;

type ExtractComparable<V, C> = fn(&V) -> C;

/// An `OrderedMap` is like a `std::collections::HashMap`,
/// but it is sorted according to the value in descending order.
/// It doesn't require the value of the map, `V`, to be comparable,
/// the comparison of the value is done on `C`,
/// which is the return value of `extract_comparable(&V)`.
#[derive(Clone)]
pub struct OrderedMap<K, V, C> {
    map: HashMap<K, V>,

    descending_pairs: Vec<(K, C)>,

    extract_comparable: ExtractComparable<V, C>,
}

impl<K: fmt::Debug, V: fmt::Debug, C: fmt::Debug> fmt::Debug for OrderedMap<K, V, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("OrderedMap")
            .field("map", &self.map)
            .field("descending_pairs", &self.descending_pairs)
            .finish()
    }
}

pub struct DescendingKeys<'a, K: 'a, C: 'a> {
    inner: std::slice::Iter<'a, (K, C)>,
}

impl<'a, K: 'a, C: 'a> Iterator for DescendingKeys<'a, K, C> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            None => None,
            Some((k, _)) => Some(k),
        }
    }
}

impl<'a, K: 'a, C: 'a> DoubleEndedIterator for DescendingKeys<'a, K, C> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.inner.next_back() {
            None => None,
            Some((k, _)) => Some(k),
        }
    }
}

pub struct DescendingValues<'a, K, V, C> {
    map: &'a HashMap<K, V>,
    keys: DescendingKeys<'a, K, C>,
}

impl<'a, K, V, C> Iterator for DescendingValues<'a, K, V, C>
where
    K: Eq + Hash,
{
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        match self.keys.next() {
            None => None,
            Some(k) => Some(self.map.index(k)),
        }
    }
}

impl<'a, K, V, C> DoubleEndedIterator for DescendingValues<'a, K, V, C>
where
    K: Eq + Hash,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.keys.next_back() {
            None => None,
            Some(k) => Some(self.map.index(k)),
        }
    }
}

pub struct DescendingItems<'a, K, V, C> {
    map: &'a HashMap<K, V>,
    keys: DescendingKeys<'a, K, C>,
}

impl<'a, K, V, C> Iterator for DescendingItems<'a, K, V, C>
where
    K: Eq + Hash,
{
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        match self.keys.next() {
            None => None,
            Some(k) => Some((k, self.map.index(k))),
        }
    }
}

impl<'a, K, V, C> DoubleEndedIterator for DescendingItems<'a, K, V, C>
where
    K: Eq + Hash,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.keys.next_back() {
            None => None,
            Some(k) => Some((k, self.map.index(k))),
        }
    }
}

impl<'a, K: 'a, V: 'a, C: 'a> OrderedMap<K, V, C>
where
    K: Eq + Hash + Copy,
    C: PartialOrd,
{
    /// The function `extract_comparable` is used to convert the value of type `&V`
    /// to something comparable of type `C`
    pub fn new(extract_comparable: ExtractComparable<V, C>) -> Self {
        OrderedMap {
            map: HashMap::new(),
            descending_pairs: vec![],
            extract_comparable,
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Keys of this map in descending order
    pub fn descending_keys(&'a self) -> DescendingKeys<'a, K, C> {
        DescendingKeys {
            inner: self.descending_pairs.iter(),
        }
    }

    /// Values of this map in descending order
    pub fn descending_values(&'a self) -> DescendingValues<'a, K, V, C> {
        DescendingValues {
            map: &self.map,
            keys: self.descending_keys(),
        }
    }

    /// (K, V) pairs of this map in descending order
    pub fn descending_items(&'a self) -> DescendingItems<'a, K, V, C> {
        DescendingItems {
            map: &self.map,
            keys: self.descending_keys(),
        }
    }

    fn insert_into_pairs(&mut self, k: K, c: C) {
        let mut insert_index = None;
        for (i, (_ek, ec)) in self.descending_pairs.iter().enumerate() {
            if &c >= ec {
                insert_index = Some(i);
                break;
            }
        }
        let idx = match insert_index {
            None => self.descending_pairs.len(),
            Some(i) => i,
        };
        self.descending_pairs.insert(idx, (k, c));
    }

    /// Insert a new key-value pair to the map,
    /// the old value is returned as `Option<V>`
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let new_c = (self.extract_comparable)(&v);
        match self.map.insert(k, v) {
            None => {
                self.insert_into_pairs(k, new_c);
                None
            }
            Some(v) => {
                remove_from_pairs(&mut self.descending_pairs, &k);
                self.insert_into_pairs(k, new_c);
                Some(v)
            }
        }
    }

    /// Remove a key-value pair from the map
    pub fn remove(&mut self, k: &K) -> Option<V> {
        match self.map.remove(k) {
            None => None,
            Some(v) => {
                remove_from_pairs(&mut self.descending_pairs, k);
                Some(v)
            }
        }
    }

    /// Get a value from the map
    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k)
    }
}

fn remove_from_pairs<K, C>(pairs: &mut Vec<(K, C)>, k: &K) -> bool
where
    K: Eq,
{
    let mut removed = false;
    for i in 0..pairs.len() {
        if pairs[i].0 == *k {
            pairs.remove(i);
            removed = true;
            break;
        }
    }
    removed
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::OrderedMap;

    fn to_comparable(t: &(f32, f64)) -> f32 {
        t.0
    }

    #[quickcheck]
    fn descending_order(kvs: Vec<(i32, (f32, f64))>) -> bool {
        let empty = kvs.is_empty();

        let ks: Vec<i32> = kvs.iter().map(|(k, _)| k.clone()).collect();
        let vs: Vec<(f32, f64)> = kvs.iter().map(|(_, v)| v.clone()).collect();

        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in ks.iter().zip(vs.iter()) {
            map.insert(k.clone(), v.clone());
        }

        let mut tuples: Vec<(i32, f32)> = ks
            .iter()
            .zip(vs.iter())
            .map(|(k, v)| (k.clone(), to_comparable(v)))
            .collect();
        let mut count = HashMap::new();
        for k in ks.iter() {
            count.insert(k, 0);
        }
        for k in ks.iter() {
            count.insert(k, count.get(k).unwrap() + 1);
        }
        let mut i = 0;
        for _ in 0..tuples.len() {
            if i < tuples.len() {
                let (k, _c) = tuples[i];
                let cnt = count.get_mut(&k).unwrap();
                if *cnt > 1 {
                    tuples.remove(i);
                    *cnt = *cnt - 1;
                } else {
                    i = i + 1;
                }
            } else {
                break;
            }
        }
        tuples.sort_by(|(_, c1), (_, c2)| c1.partial_cmp(c2).unwrap());
        tuples.reverse();

        let truth_keys: Vec<i32> = tuples.iter().map(|(k, _)| k.clone()).collect();

        let have_keys: Vec<i32> = map.descending_keys().map(|x| x.clone()).collect();

        let property = truth_keys == have_keys;

        let safe1 = empty || !truth_keys.is_empty();

        property && safe1
    }

    #[quickcheck]
    fn same_length(kvs: Vec<(i32, (f32, f64))>) -> bool {
        let ks: Vec<i32> = kvs.iter().map(|(k, _)| k.clone()).collect();
        let vs: Vec<(f32, f64)> = kvs.iter().map(|(_, v)| v.clone()).collect();

        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in ks.iter().zip(vs.iter()) {
            map.insert(k.clone(), v.clone());
        }

        map.map.len() == map.descending_keys().collect::<Vec<_>>().len()
    }

    #[quickcheck]
    fn same_keys(kvs: Vec<(i32, (f32, f64))>) -> bool {
        let ks: Vec<i32> = kvs.iter().map(|(k, _)| k.clone()).collect();
        let vs: Vec<(f32, f64)> = kvs.iter().map(|(_, v)| v.clone()).collect();

        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in ks.iter().zip(vs.iter()) {
            map.insert(k.clone(), v.clone());
        }

        let mut a = map.descending_keys().map(|x| x.clone()).collect::<Vec<_>>();
        a.sort();

        let mut b = map.map.keys().map(|x| x.clone()).collect::<Vec<_>>();
        b.sort();

        let mut ks = ks;
        ks.sort();
        ks.dedup();

        a == b && b == ks
    }

    #[quickcheck]
    fn insert_then_remove_all_is_empty(kvs: Vec<(i32, (f32, f64))>, other_keys: Vec<i32>) -> bool {
        let ks: Vec<i32> = kvs.iter().map(|(k, _)| k.clone()).collect();
        let vs: Vec<(f32, f64)> = kvs.iter().map(|(_, v)| v.clone()).collect();

        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in ks.iter().zip(vs.iter()) {
            map.insert(k.clone(), v.clone());
        }

        for k in ks.iter() {
            map.remove(k);
        }

        let a = 0 == map.map.len() && 0 == map.descending_keys().collect::<Vec<_>>().len();

        for k in other_keys.iter() {
            map.remove(k);
        }

        let b = 0 == map.map.len() && 0 == map.descending_keys().collect::<Vec<_>>().len();

        a && b
    }

    #[quickcheck]
    fn insert_then_remove_is_identity(kvs: Vec<(u32, (f32, f64))>, new_v: (f32, f64)) -> bool {
        let ks: Vec<u32> = kvs.iter().map(|(k, _)| k.clone()).collect();
        let vs: Vec<(f32, f64)> = kvs.iter().map(|(_, v)| v.clone()).collect();

        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in ks.iter().zip(vs.iter()) {
            map.insert(k.clone(), v.clone());
        }

        let old_map = map.map.clone();
        let old_keys = map
            .descending_keys()
            .map(|k| k.clone())
            .collect::<Vec<u32>>();

        // create a unique key
        let k: u32 = ks.iter().sum();
        let k: u32 = k + 1;
        map.insert(k.clone(), new_v);
        map.remove(&k);

        let new_map = map.map.clone();
        let new_keys = map.descending_keys().map(|k| k.clone()).collect::<Vec<_>>();

        old_map == new_map && old_keys == new_keys
    }

    #[quickcheck]
    fn insert_and_get(key: u32, val: (f32, f64)) -> bool {
        let mut map = OrderedMap::new(to_comparable);
        map.insert(key, val);
        let res = map.get(&key).unwrap();
        res == &val
    }
}
