use std::hash::Hash;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

use std::collections::HashMap;
use std::vec::Vec;

pub struct OrderedMap<K, V, C, F>
where
    F: Fn(&V) -> C,
{
    map: HashMap<K, V>,

    descendings: Vec<(K, C)>,

    to_comparable: F
}


impl<K, V, C, F> OrderedMap<K, V, C, F>
where
    K: Eq + Hash + Copy,
    C: PartialOrd,
    F: Fn(&V) -> C,
{
    pub fn new(to_comparable: F) -> Self
        where
            F: Fn(&V) -> C
    {
        OrderedMap {
            map: HashMap::new(),
            descendings: vec![],
            to_comparable: to_comparable,
        }
    }


    pub fn map(&self) -> &HashMap<K, V> {
        &self.map
    } 

    pub fn descending_keys(&self) -> impl Iterator<Item = K> + '_
    {
        self.descendings.iter().map(|(k, _c)| k.clone())
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

    pub fn insert(&mut self, k: K, v: V) {
        let new_c = (self.to_comparable)(&v);
        let old_len = self.descendings.len();
        match self.map.insert(k, v) {
            None => self.insert_only(k, new_c),
            Some(_) => {
                for i in 0..old_len {
                    if self.descendings[i].0 == k {
                        self.descendings.remove(i);
                        break
                    }
                }
                self.insert_only(k, new_c)
            }
        }
    }

}
