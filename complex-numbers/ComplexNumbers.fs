module ComplexNumbers

// https://www.mathsisfun.com/numbers/complex-numbers.html

open System

type ComplexNumber = { Real: float; Imaginary: float }


let create real imaginary = { Real = real; Imaginary = imaginary }

let mul z1 z2 =
    { Real =
          (z1.Real * z2.Real)
          + (z1.Imaginary * z2.Imaginary * -1.)
      Imaginary =
          (z1.Real * z2.Imaginary)
          + (z1.Imaginary * z2.Real) }

let add z1 z2 =
    { Real = z1.Real + z2.Real
      Imaginary = z1.Imaginary + z2.Imaginary }

let sub z1 z2 =
    { Real = z1.Real - z2.Real
      Imaginary = z1.Imaginary - z2.Imaginary }

let abs z =
    sqrt (pown z.Real 2 + pown z.Imaginary 2)

let conjugate z =
    { Real = z.Real
      Imaginary = z.Imaginary * -1. }

let div z1 z2 =
    let top = mul z1 (conjugate z2)
    let bottom = mul z2 (conjugate z2)
    { Real = top.Real / bottom.Real
      Imaginary = top.Imaginary / bottom.Real }

let real z = z.Real

let imaginary z = z.Imaginary

let exp z =
    { Real = Math.Exp(z.Real) * Math.Cos(z.Imaginary)
      Imaginary = Math.Exp(z.Real) * Math.Sin(z.Imaginary) }
