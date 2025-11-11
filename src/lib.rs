use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::hash::Hash;
use std::iter::DoubleEndedIterator;
use std::ops::Index;

type ExtractComparable<V, C> = fn(&V) -> C;

/// An `OrderedMap` is like a `std::collections::HashMap`,
/// but it is sorted according to the value in descending order.
/// It doesn't require the value of the map, `V`, to be comparable,
/// the comparison of the value is done on `C`,
/// which is the return value of `extract_comparable(&V)`.
#[derive(Clone)]
pub struct OrderedMap<K, V, C> {
    map: HashMap<K, V>,
    descending_pairs: BTreeMap<C, Vec<K>>,
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
    outer: std::iter::Rev<std::collections::btree_map::Iter<'a, C, Vec<K>>>,
    current: Option<std::slice::Iter<'a, K>>,
}

impl<'a, K: 'a, C: 'a> Iterator for DescendingKeys<'a, K, C> {
    type Item = &'a K;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut inner) = self.current {
                if let Some(k) = inner.next() {
                    return Some(k);
                }
            }
            match self.outer.next() {
                Some((_, keys)) => self.current = Some(keys.iter()),
                None => return None,
            }
        }
    }
}

impl<'a, K: 'a, C: 'a> DoubleEndedIterator for DescendingKeys<'a, K, C> {
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(ref mut inner) = self.current {
                if let Some(k) = inner.next_back() {
                    return Some(k);
                }
            }
            match self.outer.next_back() {
                Some((_, keys)) => self.current = Some(keys.iter()),
                None => return None,
            }
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
    C: Ord,
{
    /// The function `extract_comparable` is used to convert the value of type `&V`
    /// to something comparable of type `C`
    pub fn new(extract_comparable: ExtractComparable<V, C>) -> Self {
        OrderedMap {
            map: HashMap::new(),
            descending_pairs: BTreeMap::new(),
            extract_comparable,
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Keys of this map in descending order
    pub fn descending_keys(&'a self) -> DescendingKeys<'a, K, C> {
        DescendingKeys {
            outer: self.descending_pairs.iter().rev(),
            current: None,
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

    /// Get a reference to the value corresponding to the key
    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k)
    }

    /// Insert a new key-value pair to the map,
    /// the old value is returned as `Option<V>`
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let new_c = (self.extract_comparable)(&v);

        if let Some(old_v) = self.map.get(&k) {
            let old_c = (self.extract_comparable)(old_v);

            if old_c == new_c {
                return self.map.insert(k, v);
            }

            if let Some(keys) = self.descending_pairs.get_mut(&old_c) {
                keys.retain(|&existing_k| existing_k != k);
                if keys.is_empty() {
                    self.descending_pairs.remove(&old_c);
                }
            }
        }

        self.descending_pairs.entry(new_c).or_default().push(k);
        self.map.insert(k, v)
    }

    /// Remove a key-value pair from the map
    pub fn remove(&mut self, k: &K) -> Option<V> {
        let old = self.map.remove(k);

        if let Some(old) = &old {
            let old_c = (self.extract_comparable)(old);
            if let Some(keys) = self.descending_pairs.get_mut(&old_c) {
                keys.retain(|existing_k| existing_k != k);
                if keys.is_empty() {
                    self.descending_pairs.remove(&old_c);
                }
            }
        };

        old
    }
}

impl<K, V> Default for OrderedMap<K, V, V>
where
    V: Ord + Clone,
{
    fn default() -> Self {
        Self {
            map: Default::default(),
            descending_pairs: Default::default(),
            extract_comparable: |v| v.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::OrderedMap;
    use quickcheck_macros::quickcheck;
    use std::collections::HashMap;

    fn to_comparable(t: &(i32, i64)) -> i32 {
        t.0
    }

    #[quickcheck]
    fn descending_order(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);

        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let mut deduped: HashMap<i8, i16> = HashMap::new();
        for (k, (c, _)) in kvs.iter() {
            deduped.insert(*k, *c);
        }

        let mut expected: Vec<(i8, i16)> = deduped.into_iter().collect();
        expected.sort_by(|(k1, c1), (k2, c2)| c2.cmp(c1).then(k1.cmp(k2)));

        let mut actual: Vec<(i8, i16)> = map
            .descending_keys()
            .map(|&k| (k, map.map.get(&k).map(|v| v.0).unwrap()))
            .collect();
        actual.sort_by(|(k1, c1), (k2, c2)| c2.cmp(c1).then(k1.cmp(k2)));

        actual == expected
    }

    #[quickcheck]
    fn same_length(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }
        let descending_count = map.descending_keys().collect::<Vec<_>>().len();
        map.map.len() == descending_count
    }

    #[quickcheck]
    fn same_keys(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);

        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let mut descending = map.descending_keys().copied().collect::<Vec<_>>();
        descending.sort();

        let mut hashmap_keys = map.map.keys().copied().collect::<Vec<_>>();
        hashmap_keys.sort();

        descending == hashmap_keys
    }

    #[quickcheck]
    fn insert_then_remove_all_is_empty(kvs: Vec<(i8, (i16, i64))>, other_keys: Vec<i8>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);

        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let all_keys: Vec<i8> = map.map.keys().copied().collect();
        for k in all_keys {
            map.remove(&k);
        }

        let is_empty = map.map.is_empty() && map.descending_keys().collect::<Vec<_>>().is_empty();

        for k in other_keys.iter() {
            map.remove(k);
        }

        let still_empty =
            map.map.is_empty() && map.descending_keys().collect::<Vec<_>>().is_empty();

        is_empty && still_empty
    }

    #[quickcheck]
    fn insert_then_remove_is_identity(kvs: Vec<(u8, (i32, i64))>, new_v: (i32, i64)) -> bool {
        let mut map = OrderedMap::new(to_comparable);

        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let old_map = map.map.clone();
        let old_keys = map.descending_keys().map(|k| *k).collect::<Vec<u8>>();

        let unique_k = (0u8..=255)
            .find(|k| !map.map.contains_key(k))
            .unwrap_or(255);
        map.insert(unique_k, new_v);
        map.remove(&unique_k);

        let new_map = map.map.clone();
        let new_keys = map.descending_keys().map(|k| *k).collect::<Vec<_>>();

        old_map == new_map && old_keys == new_keys
    }

    #[quickcheck]
    fn descending_values_match(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let keys: Vec<i8> = map.descending_keys().copied().collect();
        let values: Vec<(i16, i64)> = map.descending_values().copied().collect();

        keys.len() == values.len()
            && keys
                .iter()
                .zip(values.iter())
                .all(|(k, v)| map.get(k) == Some(v))
    }

    #[quickcheck]
    fn descending_items_match(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let items: Vec<(i8, (i16, i64))> = map.descending_items().map(|(k, v)| (*k, *v)).collect();
        let keys: Vec<i8> = map.descending_keys().copied().collect();
        let values: Vec<(i16, i64)> = map.descending_values().copied().collect();

        items.len() == keys.len()
            && items.iter().zip(keys.iter()).all(|((k1, _), k2)| k1 == k2)
            && items
                .iter()
                .zip(values.iter())
                .all(|((_, v1), v2)| v1 == v2)
    }

    #[quickcheck]
    fn reverse_iteration_keys(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let forward: Vec<i8> = map.descending_keys().copied().collect();
        let backward: Vec<i8> = map.descending_keys().rev().copied().collect();

        forward.iter().rev().eq(backward.iter())
    }

    #[quickcheck]
    fn reverse_iteration_values(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let forward: Vec<(i16, i64)> = map.descending_values().copied().collect();
        let backward: Vec<(i16, i64)> = map.descending_values().rev().copied().collect();

        forward.iter().rev().eq(backward.iter())
    }

    #[quickcheck]
    fn reverse_iteration_items(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let forward: Vec<(i8, (i16, i64))> =
            map.descending_items().map(|(k, v)| (*k, *v)).collect();
        let backward: Vec<(i8, (i16, i64))> = map
            .descending_items()
            .rev()
            .map(|(k, v)| (*k, *v))
            .collect();

        forward.iter().rev().eq(backward.iter())
    }

    #[quickcheck]
    fn get_returns_correct_value(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let mut deduped: HashMap<i8, (i16, i64)> = HashMap::new();
        for (k, v) in kvs.iter() {
            deduped.insert(*k, *v);
        }

        deduped.iter().all(|(k, v)| map.get(k) == Some(v))
    }

    #[quickcheck]
    fn len_matches_unique_keys(kvs: Vec<(i8, (i16, i64))>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);
        for (k, v) in kvs.iter() {
            map.insert(*k, *v);
        }

        let mut unique_keys: Vec<i8> = kvs.iter().map(|(k, _)| *k).collect();
        unique_keys.sort();
        unique_keys.dedup();

        map.len() == unique_keys.len()
    }

    #[quickcheck]
    fn values_preserved_through_updates(key: i8, values: Vec<(i16, i64)>) -> bool {
        let mut map = OrderedMap::new(|t: &(i16, i64)| t.0);

        for v in values.iter() {
            map.insert(key, *v);
        }

        let expected = values.last().copied();
        map.get(&key) == expected.as_ref()
    }

    #[test]
    fn default_impl_creates_empty_map() {
        let map: OrderedMap<i32, i32, i32> = OrderedMap::default();
        assert_eq!(map.len(), 0);
        assert_eq!(map.descending_keys().count(), 0);
    }

    #[test]
    fn default_impl_orders_by_value() {
        let mut map: OrderedMap<i8, i16, i16> = OrderedMap::default();
        map.insert(1, 10);
        map.insert(2, 20);
        map.insert(3, 15);

        let keys: Vec<i8> = map.descending_keys().copied().collect();
        let values: Vec<i16> = map.descending_values().copied().collect();

        assert_eq!(values, vec![20, 15, 10]);
        assert_eq!(keys, vec![2, 3, 1]);
    }
}
