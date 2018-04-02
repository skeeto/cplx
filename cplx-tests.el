;;; cplx-tests.el --- tests for cplx -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'cl-lib)
(eval-when-compile (require 'cplx))

(cl-defun nearly-eql (a b &optional (delta 1e-9))
  (or (eql a b)
      (< (/ (abs (- a b)) (min (abs a) (abs b)))
         delta)))

(cl-defun cplx-eql (a b)
  (and (nearly-eql (cplx-real a)
                   (cplx-real b))
       (nearly-eql (cplx-imag a)
                   (cplx-imag b))))

(ert-deftest cplx ()
  (let* ((re 2.1)
         (im 3.2)
         (c (cplx re im)))
    (should (eq  (cplx-p c) t))
    (should (eql (cplx-real c) re))
    (should (eql (cplx-imag c) im))))

;; The constants below all come from GNU Octave:

(ert-deftest cplx-abs ()
  (let ((c (cplx 2.0 3.0)))
    (should (nearly-eql (cplx-abs c) 3.60555127546399))))

(ert-deftest cplx-arg ()
  (let ((c (cplx 2.0 3.0)))
    (should (nearly-eql (cplx-arg c) 0.982793723247329))))

(ert-deftest cplx-conj ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-conj c)
                      (cplx 2.0 -3.0)))))

(ert-deftest cplx-proj ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-proj c)
                      (cplx 0.554700196225229 0.832050294337844)))))

(ert-deftest cplx-sqrt ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-sqrt c)
                      (cplx 1.67414922803554 0.895977476129838)))))

(ert-deftest cplx-sin ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-sin c)
                      (cplx 9.15449914691143 -4.16890695996656)))))

(ert-deftest cplx-cos ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-cos c)
                      (cplx -4.18962569096881 -9.10922789375534)))))

(ert-deftest cplx-tan ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-tan c)
                      (cplx -0.00376402564150425 1.00323862735361)))))

(ert-deftest cplx-log ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-log c)
                      (cplx 1.28247467873077 0.982793723247329)))))

(ert-deftest cplx-asin ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-asin c)
                      (cplx 0.570652784321099 1.98338702991654)))))

(ert-deftest cplx-acos ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-acos c)
                      (cplx 1.0001435424738 -1.98338702991654)))))

(ert-deftest cplx-atan ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-atan c)
                      (cplx 1.40992104959658 0.229072682968539)))))

(ert-deftest cplx-exp ()
  (let ((c (cplx 2.0 3.0)))
    (should (cplx-eql (cplx-exp c)
                      (cplx -7.3151100949011 1.0427436562359)))))

(ert-deftest cplx-expt ()
  (let ((a (cplx 2.0 3.0))
        (b (cplx -2.1 3.2)))
    (should (cplx-eql (cplx-expt a b)
                      (cplx -0.0013178979037822 0.00259925004826532)))))

;;; cplx-tests.el ends here
