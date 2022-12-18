//! Extension's to Smithay's geometry module.

use std::error::Error;
use std::str::FromStr;
use std::{cmp, ops};

use smithay::utils::{Point, Size};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Matrix3x3<T: Copy> {
    storage: Vec<T>,
}

impl<T: Copy> TryFrom<Vec<T>> for Matrix3x3<T> {
    type Error = Box<dyn Error>;

    fn try_from(storage: Vec<T>) -> Result<Self, Self::Error> {
        if storage.len() == 9 {
            Ok(Self { storage })
        } else {
            let error = format!(
                "Mismatched size when creating Matrix3x3 from Vec, expected length 9, got {}",
                storage.len()
            );
            Err(error.into())
        }
    }
}

impl ops::Mul<Vector3D<f32>> for &Matrix3x3<f32> {
    type Output = Vector3D<f32>;

    fn mul(self, rhs: Vector3D<f32>) -> Self::Output {
        let x = self.storage[0] * rhs.x + self.storage[1] * rhs.y + self.storage[2] * rhs.z;
        let y = self.storage[3] * rhs.x + self.storage[4] * rhs.y + self.storage[5] * rhs.z;
        let z = self.storage[6] * rhs.x + self.storage[7] * rhs.y + self.storage[8] * rhs.z;
        Self::Output { x, y, z }
    }
}

// The expected format is "0, 0, 0; 0, 0, 0; 0, 0, 0".
impl<T: FromStr + Copy> FromStr for Matrix3x3<T>
where
    <T as FromStr>::Err: Error + 'static,
{
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rows = s
            .split(';')
            // Ensure that we process only 3 rows.
            .take(3)
            // Take only 3 element from each row.
            .flat_map(|row| row.split(',').take(3))
            .map(|ch| T::from_str(ch.trim()))
            .collect::<Result<Vec<T>, _>>()?;

        Self::try_from(rows)
    }
}

/// A point in the 3 dimensional space.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Vector3D<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T> Vector3D<T> {
    #[inline]
    pub fn new(x: T, y: T, z: T) -> Self {
        Self { x, y, z }
    }
}

impl Vector for Vector3D<f32> {
    fn scale(&self, scale: f64) -> Self {
        let x = scale as f32 * self.x;
        let y = scale as f32 * self.y;
        let z = scale as f32 * self.z;
        Self { x, y, z }
    }

    fn min(&self, other: impl Into<Self>) -> Self {
        let other = other.into();
        let x = self.x.min(other.x);
        let y = self.y.min(other.y);
        let z = self.z.min(other.z);
        Self { x, y, z }
    }

    fn max(&self, other: impl Into<Self>) -> Self {
        let other = other.into();
        let x = self.x.max(other.x);
        let y = self.y.max(other.y);
        let z = self.z.max(other.z);
        Self { x, y, z }
    }

    fn sub(&self, other: impl Into<Self>) -> Self {
        let other = other.into();
        Self { x: self.x - other.x, y: self.y - other.y, z: self.z - other.z }
    }
}

pub trait Vector: Sized {
    /// Scale the size by a scaling factor.
    fn scale(&self, scale: f64) -> Self;

    /// Compare and return the smaller of each individual dimensions.
    fn min(&self, other: impl Into<Self>) -> Self;

    /// Compare and return the bigger of each individual dimensions.
    fn max(&self, other: impl Into<Self>) -> Self;

    /// Subtract one vector from another.
    fn sub(&self, other: impl Into<Self>) -> Self;
}

/// Helper trait for converting into a 2D vector.
pub trait IntoVector {
    fn as_vector(&self) -> (i32, i32);
}

impl<K> IntoVector for Point<i32, K> {
    fn as_vector(&self) -> (i32, i32) {
        (self.x, self.y)
    }
}

impl<K> IntoVector for Size<i32, K> {
    fn as_vector(&self) -> (i32, i32) {
        (self.w, self.h)
    }
}

impl<T> Vector for T
where
    T: IntoVector,
    T: From<(i32, i32)>,
{
    fn scale(&self, scale: f64) -> Self {
        let tuple = self.as_vector();
        Self::from((
            (tuple.0 as f64 * scale).round() as i32,
            (tuple.1 as f64 * scale).round() as i32,
        ))
    }

    fn min(&self, other: impl Into<Self>) -> Self {
        let tuple = self.as_vector();
        let other = other.into().as_vector();
        Self::from((cmp::min(tuple.0, other.0), cmp::min(tuple.1, other.1)))
    }

    fn max(&self, other: impl Into<Self>) -> Self {
        let tuple = self.as_vector();
        let other = other.into().as_vector();
        Self::from((cmp::max(tuple.0, other.0), cmp::max(tuple.1, other.1)))
    }

    fn sub(&self, other: impl Into<Self>) -> Self {
        let lhs = self.as_vector();
        let rhs = other.into().as_vector();
        Self::from((lhs.0 - rhs.0, lhs.1 - rhs.1))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_matrix() {
        let matrix = "1, 0, 0; 0, 1, 0; 0, 0, 1";
        assert_eq!(
            vec![1, 0, 0, 0, 1, 0, 0, 0, 1],
            Matrix3x3::<i32>::from_str(matrix).unwrap().storage
        );

        let matrix = "1, 0, 0, 0, 1, 0, 0, 0, 1";
        assert!(Matrix3x3::<i32>::from_str(matrix).is_err());

        let matrix = "1; 0; 0; 0; 1; 0; 0; 0; 1;";
        assert!(Matrix3x3::<i32>::from_str(matrix).is_err());
    }
}
