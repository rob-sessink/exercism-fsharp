module ComplexNumbers

// https://www.mathsisfun.com/numbers/complex-numbers.html

open System

type ComplexNumber = { real: float; imaginary: float }


let create real imaginary = { real = real; imaginary = imaginary }

let mul z1 z2 =
    { real =
          (z1.real * z2.real)
          + (z1.imaginary * z2.imaginary * -1.)
      imaginary =
          (z1.real * z2.imaginary)
          + (z1.imaginary * z2.real) }

let add z1 z2 =
    { real = z1.real + z2.real
      imaginary = z1.imaginary + z2.imaginary }

let sub z1 z2 =
    { real = z1.real - z2.real
      imaginary = z1.imaginary - z2.imaginary }

let abs z =
    sqrt (pown z.real 2 + pown z.imaginary 2)

let conjugate z =
    { real = z.real
      imaginary = z.imaginary * -1. }

let div z1 z2 =
    let top = mul z1 (conjugate z2)
    let bottom = mul z2 (conjugate z2)
    { real = top.real / bottom.real
      imaginary = top.imaginary / bottom.real }

let real z = z.real

let imaginary z = z.imaginary

let exp z =
    { real = Math.Exp(z.real) * Math.Cos(z.imaginary)
      imaginary = Math.Exp(z.real) * Math.Sin(z.imaginary) }
