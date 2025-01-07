use nalgebra::{DMatrix, Dyn, OMatrix};
use pest::iterators::Pairs;

use crate::{
    expr::{parse_expr, Expression},
    ParserError, Program, Rule,
};

pub(crate) type Matrix<T> = OMatrix<T, Dyn, Dyn>;

impl Program {
    pub(crate) fn parse_matrix(
        &self,
        rows: Pairs<Rule>,
    ) -> Result<Matrix<Expression>, ParserError> {
        let lengths = rows.clone().fold(None, |size, row| match size {
            None => Some(Ok(row.into_inner().count())),
            Some(Ok(count)) => {
                if count == row.into_inner().count() {
                    Some(Ok(count))
                } else {
                    Some(Err(ParserError::MalformedMatrix))
                }
            }
            Some(err) => Some(err),
        });
        let column_count = match lengths {
            None => return Err(ParserError::EmptyMatrix),
            Some(Ok(count)) => count,
            Some(Err(err)) => return Err(err),
        };
        let mat = DMatrix::from_row_iterator(
            rows.clone().count(),
            column_count,
            rows.flat_map(|row| {
                row.into_inner().map(|item| {
                    parse_expr(&mut item.into_inner())
                        .expect("expression in matrix should be valid")
                })
            }),
        );

        Ok(mat)
    }
}

#[cfg(test)]
mod test {
    use nalgebra::Matrix2;
    use num_complex::Complex64;
    use pest::Parser;

    use crate::{expr::Expr, ParserError, Program, QParser, Rule};

    #[test]
    fn test_matrix_parse() {
        let input = "[[
            1,-2,
            3i,4 + 1i,
        ]]
        ";
        let mut pairs = QParser::parse(Rule::matrix, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(
            Program::default()
                .parse_matrix(pairs.next().unwrap().into_inner())
                .unwrap(),
            Matrix2::from_row_slice(&[
                Expr::Res(Complex64::new(1.0, 0.0)),
                Expr::Res(Complex64::new(-2.0, 0.0)),
                Expr::Res(Complex64::new(0.0, 3.0)),
                Expr::Res(Complex64::new(4.0, 1.0)),
            ])
        )
    }

    #[test]
    fn test_matrix_error_malformed_1() {
        let input = "[[
            1,-2,
            3i,
        ]]
        ";
        let mut pairs = QParser::parse(Rule::matrix, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(
            Program::default()
                .parse_matrix(pairs.next().unwrap().into_inner())
                .unwrap_err(),
            ParserError::MalformedMatrix
        )
    }

    #[test]
    fn test_matrix_error_malformed_2() {
        let input = "[[
            1,
            3i,-2,
        ]]
        ";
        let mut pairs = QParser::parse(Rule::matrix, input).unwrap();
        println!("pairs: {pairs:#?}");
        assert_eq!(
            Program::default()
                .parse_matrix(pairs.next().unwrap().into_inner())
                .unwrap_err(),
            ParserError::MalformedMatrix
        )
    }
}
