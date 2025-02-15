use extendr_api::prelude::*;

/// Verify the matrix type works
/// @export
/// @noRd
#[extendr]
fn describe_matrix(matrix: ArrayView2<f64>){
    println!("This R matrix has shape {:?}", matrix.dim())
}

/// Verify the vector type works
/// @export
/// @noRd
#[extendr]
fn describe_vector(vector: ArrayView1<f64>){
    println!("This R vector has length {:?}", vector.len())
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod Coereba;
    fn describe_matrix;
    fn describe_vector;
}
