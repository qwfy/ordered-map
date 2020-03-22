use std::vec::Vec;
use std::hash::Hash;
use std::collections::HashMap;


#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

type ToComparable<V, C> = fn(&V) -> C;

pub struct OrderedMap<K, V, C>
{
    map: HashMap<K, V>,

    descendings: Vec<(K, C)>,

    to_comparable: ToComparable<V, C>,
}


impl<K, V, C> OrderedMap<K, V, C>
where
    K: Eq + Hash + Copy,
    C: PartialOrd,
{
    /// The function `to_comparable` is used to convert the value to something comparable
    pub fn new(to_comparable: ToComparable<V, C>) -> Self
    {
        OrderedMap {
            map: HashMap::new(),
            descendings: vec![],
            to_comparable: to_comparable,
        }
    }


    /// Retrieve the underlying unordered HashMap
    /// NOTE: You should not modify this map directly
    pub fn unordered(&self) -> &HashMap<K, V> {
        &self.map
    } 

    /// Keys of this map in descending order
    pub fn descending_keys(&self) -> impl Iterator<Item = K> + '_
    {
        self.descendings.iter().map(|(k, _c)| k.clone())
    }

    /// Keys of this map in ascending order
    pub fn ascending_keys(&self) -> impl Iterator<Item = K> + '_
    {
        self.descendings.iter().map(|(k, _c)| k.clone()).rev()
    }

    fn insert_only(&mut self, k: K, c: C) {
        let mut insert_index = None;
        for (i, (_ek, ec)) in self.descendings.iter().enumerate() {
            if &c >= ec {
                insert_index = Some(i);
                break
            }
        }
        let idx = match insert_index {
            None => self.descendings.len(),
            Some(i) => i
        };
        self.descendings.insert(idx, (k, c));
    }

    /// Insert a new key-value pair to the map
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let new_c = (self.to_comparable)(&v);
        let old_len = self.descendings.len();
        match self.map.insert(k, v) {
            None => {
                self.insert_only(k, new_c);
                None
            },
            Some(v) => {
                for i in 0..old_len {
                    if self.descendings[i].0 == k {
                        self.descendings.remove(i);
                        break
                    }
                }
                self.insert_only(k, new_c);
                Some(v)
            }
        }
    }

    /// Remove a key-value pair from the map
    pub fn remove(&mut self, k: &K) -> Option<V> {
        match self.map.remove(k) {
            None => None,
            Some(v) => {
                for i in 0..self.descendings.len() {
                    if self.descendings[i].0 == *k {
                        self.descendings.remove(i);
                        break
                    }
                };
                Some(v)
            }
        }
    }

}


#[cfg(test)]
mod tests {
    use super::OrderedMap;
    use std::collections::HashMap;

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

        let mut tuples: Vec<(i32, f32)> = ks.iter().zip(vs.iter())
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
                break
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

        map.unordered().len() == map.descending_keys().collect::<Vec<_>>().len()
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

        let mut b = map.unordered().keys().map(|x| x.clone()).collect::<Vec<_>>();
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

        let a = 0 == map.unordered().len() && 0 == map.descending_keys().collect::<Vec<_>>().len();

        for k in other_keys.iter() {
            map.remove(k);
        }

        let b = 0 == map.unordered().len() && 0 == map.descending_keys().collect::<Vec<_>>().len();

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

        let old_map = map.unordered().clone();
        let old_keys = map.descending_keys().collect::<Vec<_>>().clone();

        // create a unique key
        let k: u32 = ks.iter().sum();
        let k: u32 = k + 1;
        map.insert(k.clone(), new_v);
        map.remove(&k);

        let new_map = map.unordered().clone();
        let new_keys = map.descending_keys().collect::<Vec<_>>().clone();

        old_map == new_map && old_keys == new_keys
    }
}
