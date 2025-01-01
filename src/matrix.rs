#[derive(Debug, Default)]
pub(crate) struct Matrix<T> {
    rows: Vec<MatrixRow<T>>,
}

impl<T> Matrix<T> {
    pub(crate) fn from_iters<I, W>(iter: I) -> Self
    where
        I: IntoIterator<Item = W>,
        W: IntoIterator<Item = T>,
    {
        let mut mat = Vec::new();
        let mut len = None;
        for row in iter {
            let matrow = row.into_iter().collect::<MatrixRow<_>>();
            if let None = len {
                len = Some(matrow.len());
            } else if len != Some(matrow.len()) {
                todo!("not all rows are the same size");
            }
            mat.push(matrow);
        }
        Matrix { rows: mat }
    }
}

#[derive(Debug, Default)]
struct MatrixRow<T> {
    elements: Vec<T>,
}

impl<T> FromIterator<T> for MatrixRow<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        MatrixRow {
            elements: iter.into_iter().collect(),
        }
    }
}

impl<T> MatrixRow<T> {
    fn len(&self) -> usize {
        self.elements.len()
    }
}
