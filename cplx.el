;;; cplx.el --- complex number library -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; See README.md.

;;; Code:

(defsubst cplx (real imag)
  "Create a new complex number with the given REAL and IMAG parts."
  (cons real imag))

(defmacro cplx-const (real imag)
  "Defines a complex number constant.
Both REAL and IMAG will be evaluated at compile time and should
usually be floating point constants, though global constants may
also be reasonable."
  `(eval-when-compile (cons ,real ,imag)))

(defsubst cplx-p (object)
  "Return non-nil if OBJECT is a complex number."
  (and (consp object)
       (numberp (car object))
       (numberp (cdr object))))

(defsubst cplx-real (c)
  "Return the real component of complex number C."
  (car c))

(defsubst cplx-imag (c)
  "Return the imaginary component of complex number C."
  (cdr c))

;; Operators

(defsubst cplx-+ (a b)
  "Return the sum of complex numbers A and B."
  (cplx (+ (cplx-real a) (cplx-real b))
        (+ (cplx-imag a) (cplx-imag b))))

(defsubst cplx-- (a b)
  "Return the difference of complex numbers A and B."
  (cplx (- (cplx-real a) (cplx-real b))
        (- (cplx-imag a) (cplx-imag b))))

(defsubst cplx-* (a b)
  "Return the product of complex numbers A and B."
  (let ((ar (cplx-real a))
        (ai (cplx-imag a))
        (br (cplx-real b))
        (bi (cplx-imag b)))
    (cplx (- (* ar br) (* ai bi))
          (+ (* ar bi) (* ai br)))))

(defsubst cplx-/ (a b)
  "Return the quotient of the complex numbers A and B."
  (let* ((ar (cplx-real a))
         (ai (cplx-imag a))
         (br (cplx-real b))
         (bi (- (cplx-imag b)))
         (d (+ (* br br) (* bi bi))))
    (cplx (/ (- (* ar br) (* ai bi)) d)
          (/ (+ (* ar bi) (* ai br)) d))))

;; Functions

(defsubst cplx-abs (c)
  "Return the magnitude of the complex number C."
  (let ((re (cplx-real c))
        (im (cplx-imag c)))
    (sqrt (+ (* re re) (* im im)))))

(defsubst cplx-arg (c)
  "Return the argument of the complex number C."
  (let ((re (cplx-real c))
        (im (cplx-imag c)))
    (atan im re)))

(defsubst cplx-conj (c)
  "Return the complex conjugate of complex number C."
  (cplx (cplx-real c) (- (cplx-imag c))))

(defsubst cplx-proj (c)
  "Return the complex projection of C onto the unit sphere."
  (let ((abs (cplx-abs c)))
    (cplx (/ (cplx-real c) abs)
          (/ (cplx-imag c) abs))))

(defsubst cplx-sqrt (c)
  "Return the square root of the complex number C."
  (let* ((re (cplx-real c))
         (im (cplx-imag c))
         (f (expt (+ (* re re) (* im im)) 0.25))
         (a (/ (atan im re) 2)))
    (cplx (* f (cos a))
          (* f (sin a)))))

(defsubst cplx-sin (c)
  "Return the sine of the complex number C."
  (let ((re (cplx-real c))
        (im (cplx-imag c)))
    (cplx (* (sin re) (/ (+ (exp im) (exp (- im))) 2.0))
          (* (cos re) (/ (- (exp im) (exp (- im))) 2.0)))))

(defsubst cplx-cos (c)
  "Return the cosine of the complex number C."
  (let ((re (cplx-real c))
        (im (cplx-imag c)))
    (cplx    (* (cos re) (/ (+ (exp im) (exp (- im))) 2.0))
          (- (* (sin re) (/ (- (exp im) (exp (- im))) 2.0))))))

(defsubst cplx-tan (c)
  "Return the tangent of the complex number C."
  (cplx-/ (cplx-sin c) (cplx-cos c)))

(defsubst cplx-log (c)
  "Return the natural logarithm of the complex number C."
  (let ((re (cplx-real c))
        (im (cplx-imag c)))
    (cplx (log (sqrt (+ (* re re) (* im im))))
          (atan im re))))

(defsubst cplx-asin (c)
  "Return the arcsine of the complex number C."
  (let ((1-sq (cplx-sqrt (cplx-- (cplx-const 1.0 0.0) (cplx-* c c)))))
    (cplx-* (cplx-const 0.0 -1.0)
            (cplx-log (cplx-+ (cplx-* (cplx-const 0.0 1.0) c) 1-sq)))))

(defsubst cplx-acos (c)
  "Return the arccosine of the complex number C."
  (let ((1-sq (cplx-sqrt (cplx-- (cplx-const 1.0 0.0) (cplx-* c c)))))
    (cplx-* (cplx-const 0.0 -1.0)
            (cplx-log (cplx-+ c (cplx-* (cplx-const 0.0 1.0) 1-sq))))))

(defsubst cplx-atan (c)
  "Return the arctangent of the complex number C."
  (let ((i (cplx-const 0.0 1.0)))
    (cplx-/ (cplx-* i (cplx-log (cplx-/ (cplx-+ i c) (cplx-- i c))))
            (cplx-const 2.0 0.0))))

(defsubst cplx-exp (c)
  "Return the exponential base of the complex number C."
  (let ((im (cplx-imag c))
        (f  (exp (cplx-real c))))
    (cplx (* f (cos im))
          (* f (sin im)))))

(defsubst cplx-expt (a b)
  "Return the exponential A^B for complex numbers A and B."
  (let* ((r  (cplx-abs a))
         (p  (cplx-arg a))
         (re (cplx-real b))
         (im (cplx-imag b))
         (th (+ (* im (log r)) (* re p)))
         (f (* (expt r re) (exp (* p (- im))))))
    (cplx (* f (cos th))
          (* f (sin th)))))

(provide 'cplx)

;;; cplx.el ends here
