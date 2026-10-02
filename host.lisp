(in-package #:ctype)

;;;; These default specializations make ctype reflect the host type system.

(defmethod most-positive-fixnum (client)
  (declare (ignore client))
  cl:most-positive-fixnum)
(defmethod most-negative-fixnum (client)
  (declare (ignore client))
  cl:most-negative-fixnum)

;;; We close over the subtypeps but call eql within the zero methods,
;;; because I suspect that eql is more likely to be constant folded.
;;; Of course performance isn't going to be affected much either way.
(let ((reduction (if (cl:subtypep 'short-float 'single-float)
                     'single-float
                     'short-float)))
  (defmethod reduce-float-type (client (type (eql 'short-float)))
    (declare (ignore client))
    reduction))
(defmethod reduce-float-type (client (type (eql 'single-float)))
  (declare (ignore client))
  'single-float)
(let ((reduction (if (cl:subtypep 'double-float 'single-float)
                     'single-float
                     'double-float)))
  (defmethod reduce-float-type (client (type (eql 'double-float)))
    reduction))
(let ((reduction (cond ((cl:subtypep 'long-float 'single-float) 'single-float)
                       ((cl:subtypep 'long-float 'double-float) 'double-float)
                       (t 'long-float))))
  (defmethod reduce-float-type (client (type (eql 'long-float)))
    reduction))

(let ((distinct-float-types
        (append (if (cl:subtypep 'short-float 'single-float) () '(short-float))
                '(single-float)
                (if (cl:subtypep 'double-float 'single-float) () '(double-float))
                (if (cl:subtypep 'long-float 'double-float) () '(long-float)))))
  (defmethod distinct-float-types (client)
    (declare (ignore client))
    distinct-float-types))

(macrolet ((defzeroes (type neg pos)
             `(defmethod distinct-zeroes-p (client (type (eql ',type)))
                (declare (ignore client))
                (eql ,neg ,pos))))
  (defzeroes short-float -0S0 0S0)
  (defzeroes single-float -0F0 0F0)
  (defzeroes double-float -0D0 0D0)
  (defzeroes long-float -0L0 0L0))

(defmethod upgraded-complex-part-type (client specifier &optional env)
  (declare (ignore client))
  (cl:upgraded-complex-part-type specifier env))

(defmethod upgraded-array-element-type (client specifier &optional env)
  (declare (ignore client))
  (cl:upgraded-array-element-type specifier env))

(let ((complex-arrays-distinct-p (not (cl:subtypep 'array 'simple-array))))
  (defmethod complex-arrays-distinct-p (client)
    (declare (ignore client))
    complex-arrays-distinct-p))

(defmethod find-class (client name &optional (errorp t) env)
  (declare (ignore client))
  (cl:find-class name errorp env))

;;; Helper function that loops over all codepoints to determine ranges.
;;; Assuming code-char-limit is sane, this shouldn't take too long.
(defun find-ranges (predicate &optional (start 0) (end cl:char-code-limit))
  (loop :with ranges := nil
        :with inside-range-p := nil
        :with range-start := nil
        :with range-end := nil
        :for i :from start :below end
        :do (if (funcall predicate (code-char i))
                (if inside-range-p
                    (incf range-end)
                    (setq range-start i
                          range-end   i
                          inside-range-p t))
                (when inside-range-p
                  (progn
                    (push (cons range-start range-end) ranges)
                    (setq inside-range-p nil))))
        :finally (return (nreverse ranges))))

(let ((standard-charset-pairs (find-ranges #'standard-char-p)))
  (defmethod standard-charset-pairs (client)
    (declare (ignore client))
    standard-charset-pairs))

(let ((base-charset-pairs (find-ranges (lambda (c) (cl:typep c 'base-char)))))
  (defmethod base-charset-pairs (client)
    (declare (ignore client))
    base-charset-pairs))

(defmethod char-code-limit (client)
  (declare (ignore client))
  cl:char-code-limit)

(defmethod sfdefinition (client name)
  (declare (ignore client))
  (cl:fdefinition name))

;;; Typexpand is messy.
;;; We could leave it undefined, and leave it to programmers to define
;;; appropriate clients for their implementations, or something. But I
;;; figure many programmers want it to Just Work in the basic sense of
;;; mimicking the host type system, so we do some implementation specifics.

#+abcl
(progn
  (defun abcl-type-expand-1 (type)
    (cond ((symbolp type)
           (case type
             (BIT
              (return-from abcl-type-expand-1 '(integer 0 1)))
             (CONS
              (return-from abcl-type-expand-1 '(cons t t)))
             (FIXNUM
              (return-from abcl-type-expand-1
                '(integer #.most-negative-fixnum #.most-positive-fixnum)))
             (SIGNED-BYTE
              (return-from abcl-type-expand-1 'integer))
             (UNSIGNED-BYTE
              (return-from abcl-type-expand-1 '(integer 0 *)))
             (BASE-CHAR
              (return-from abcl-type-expand-1 'character))
             (SHORT-FLOAT
              (return-from abcl-type-expand-1 'single-float))
             (LONG-FLOAT
              (return-from abcl-type-expand-1 'double-float))
             (COMPLEX
              (return-from abcl-type-expand-1 '(complex *)))
             (ARRAY
              (return-from abcl-type-expand-1 '(array * *)))
             (SIMPLE-ARRAY
              (return-from abcl-type-expand-1 '(simple-array * *)))
             (VECTOR
              (return-from abcl-type-expand-1 '(array * (*))))
             (SIMPLE-VECTOR
              (return-from abcl-type-expand-1 '(simple-array t (*))))
             (BIT-VECTOR
              (return-from abcl-type-expand-1 '(bit-vector *)))
             (SIMPLE-BIT-VECTOR
              (return-from abcl-type-expand-1 '(simple-bit-vector *)))
             (BASE-STRING
              (return-from abcl-type-expand-1 '(array base-char (*))))
             (SIMPLE-BASE-STRING
              (return-from abcl-type-expand-1 '(simple-array base-char (*))))
             (STRING
              (return-from abcl-type-expand-1 '(string *)))
             (SIMPLE-STRING
              (return-from abcl-type-expand-1 '(simple-string *)))
             ((nil)
              (return-from abcl-type-expand-1 nil))
             (t
              (unless (get type 'deftype-definition)
                (return-from abcl-type-expand-1 type)))))
          ((classp type)
           (return-from abcl-type-expand-1
             (if (eq (class-name type) 'fixnum)
                 '(integer #.most-negative-fixnum #.most-positive-fixnum)
                 type)))
          ((and (consp type)
                (memq (car type) '(and or not eql member satisfies mod values)))
           (cond ((or (equal type '(and fixnum unsigned-byte))
                      (equal type '(and unsigned-byte fixnum)))
                  (return-from abcl-type-expand-1 '(integer 0 #.most-positive-fixnum)))
                 (t
                  (return-from abcl-type-expand-1 type)))))
    ;; Fall through...
    (let (tp i)
      (if (consp type)
          (setf tp (car type) i (cdr type))
          (setf tp type i nil))
      (when (and (symbolp tp) (get tp 'deftype-definition))
        (setf type (apply (get tp 'deftype-definition) i)))
      (case tp
        (INTEGER
         (return-from abcl-type-expand-1 (if i (cons tp i) tp)))
        (CONS
         (let* ((len (length i))
                (car-typespec (if (> len 0) (car i) t))
                (cdr-typespec (if (> len 1) (cadr i) t)))
           (unless (and car-typespec cdr-typespec)
             (return-from abcl-type-expand-1 nil))
           (when (eq car-typespec '*)
             (setf car-typespec t))
           (when (eq cdr-typespec '*)
             (setf cdr-typespec t))
           (return-from abcl-type-expand-1 (cons tp (list car-typespec cdr-typespec)))))
        (SIGNED-BYTE
         (if (or (null i) (eq (car i) '*))
             (return-from abcl-type-expand-1 'integer)
             (return-from abcl-type-expand-1
               (list 'integer
                     (- (expt 2 (1- (car i))))
                     (1- (expt 2 (1- (car i))))))))
        (UNSIGNED-BYTE
         (if (or (null i) (eq (car i) '*))
             (return-from abcl-type-expand-1 '(integer 0 *)))
         (return-from abcl-type-expand-1 (list 'integer 0 (1- (expt 2 (car i))))))
        ((ARRAY SIMPLE-ARRAY)
         (unless i
           (return-from abcl-type-expand-1 (list tp '* '*)))
         (when (= (length i) 1)
           (setf i (append i '(*))))
         (setf (car i) (abcl-type-expand-1 (car i)))
         (return-from abcl-type-expand-1 (cons tp i)))
        (VECTOR
         (case (length i)
           (0
            (return-from abcl-type-expand-1 '(array * (*))))
           (1
            (setf (car i) (abcl-type-expand-1 (car i)))
            (return-from abcl-type-expand-1 (list 'array (car i) '(*))))
           (2
            (setf (car i) (abcl-type-expand-1 (car i)))
            (return-from abcl-type-expand-1 (list 'array (car i) (list (cadr i)))))
           (t
            (error "Invalid type specifier ~S." type))))
        (SIMPLE-VECTOR
         (case (length i)
           (0
            (return-from abcl-type-expand-1 '(simple-array t (*))))
           (1
            (return-from abcl-type-expand-1 (list 'simple-array t (list (car i)))))
           (t
            (error "Invalid type specifier ~S." type))))
        (BIT-VECTOR
         (case (length i)
           (0
            (return-from abcl-type-expand-1 '(bit-vector *)))
           (1
            (return-from abcl-type-expand-1 (list 'bit-vector (car i))))
           (t
            (error "Invalid type specifier ~S." type))))
        (SIMPLE-BIT-VECTOR
         (case (length i)
           (0
            (return-from abcl-type-expand-1 '(simple-bit-vector *)))
           (1
            (return-from abcl-type-expand-1 (list 'simple-bit-vector (car i))))
           (t
            (error "Invalid type specifier ~S." type))))
        (BASE-STRING
         (if i
             (return-from abcl-type-expand-1 (list 'array 'base-char (list (car i))))
             (return-from abcl-type-expand-1 '(array base-char (*)))))
        (SIMPLE-BASE-STRING
         (if i
             (return-from abcl-type-expand-1 (list 'simple-array 'base-char (list (car i))))
             (return-from abcl-type-expand-1 '(simple-array base-char (*)))))
        (SHORT-FLOAT
         (setf tp 'single-float))
        (LONG-FLOAT
         (setf tp 'double-float))
        (COMPLEX
         (cond ((null i)
                (return-from abcl-type-expand-1 '(complex *)))
               ((eq (car i) 'short-float)
                (return-from abcl-type-expand-1 '(complex single-float)))
               ((eq (car i) 'long-float)
                (return-from abcl-type-expand-1 '(complex double-float))))))
      (if i (cons tp i) tp))))

#+ccl
(progn
  (defun ccl-type-expand-1 (form &optional env)
    (let ((def (cond ((symbolp form)
                      (gethash form ccl::%deftype-expanders%))
                     ((and (consp form) (symbolp (car form)))
                      (gethash (car form) ccl::%deftype-expanders%))
                     (t nil))))
      (if def
          (values (funcall def (if (consp form) form (list form)) env)
                  t)
          (values form nil)))))

#+cmucl
(progn
  (defun cmucl-type-expand-1 (form)
    (let ((def (cond ((symbolp form)
                      (c::info type expander form))
                     ((and (consp form) (symbolp (car form)))
                      (c::info type expander (car form)))
                     (t nil))))
      (if def
          (values (funcall def (if (consp form) form (list form)))
                  t)
          (values form
                  nil)))))

#+ecl
(progn
;;; This is like si::normalize-type, except we return a type specifier and
;;; whether it expanded, and don't signal an error if something is malformed.
;;; This obviously uses internals - fragile - but ECL doesn't export this.
(defun ecl-typexpand-1 (spec env)
  (declare (ignore env))
  (cond ((symbolp spec)
         (let ((expander (si:get-sysprop spec 'si::deftype-definition)))
           (if expander
               (values (funcall expander nil) t)
               (values spec nil))))
        ((consp spec)
         (let* ((head (car spec)) (args (cdr spec))
                (expander (si:get-sysprop head 'si::deftype-definition)))
           (if expander
               (values (funcall expander args) t)
               (values spec nil))))
        (t (values spec nil))))

(defun ecl-typexpand (type-specifier environment)
  (loop with ever-expanded = nil
        do (multiple-value-bind (expansion expandedp)
               (ecl-typexpand-1 type-specifier environment)
             (if expandedp
                 (setf ever-expanded t type-specifier expansion)
                 (return (values type-specifier ever-expanded)))))))

#+sicl
(progn
  (defun sicl-typexpand-1 (type-specifier &optional environment)
    (let* ((name (if (consp type-specifier)
                     (first type-specifier)
                     type-specifier))
           (expander (sicl-type:type-expander name)))
      (if (null expander)
          (values type-specifier nil)
          (let ((expansion (funcall expander
                                    (if (consp type-specifier)
                                        type-specifier
                                        (list type-specifier))
                                    environment)))
            (values expansion t))))))

(defmethod typexpand-1 (client tspec env)
  (declare (ignore client) (ignorable env))
  #+abcl (abcl-type-expand-1 tspec)
  #+ccl (ccl-type-expand-1 tspec env)
  #+clasp (clasp-cleavir::type-expand-1 tspec env)
  #+cmucl (cmucl-type-expand-1 tspec)
  #+ecl (ecl-typexpand-1 tspec env)
  #+sbcl (sb-ext:typexpand-1 tspec env)
  #+sicl (sicl-typexpand-1 tspec env)
  ;; in the worst case, just pretend deftype isn't real.
  ;; That will at least let you parse basic types without complaints.
  #-(or abcl ccl clasp ccl ecl sbcl sicl) tspec)

(defmethod typexpand (client tspec env)
  (declare (ignore client) (ignorable env))
  #+abcl (system:normalize-type tspec)
  #+ccl (ccl::type-expand tspec env)
  #+clasp (cleavir-env:type-expand env tspec)
  #+cmucl (kernel:type-expand tspec)
  #+ecl (ecl-typexpand tspec env)
  #+sbcl (sb-ext:typexpand tspec env)
  #+sicl (sicl-type:typexpand tspec env)
  ;; in the worst case, just pretend deftype isn't real.
  ;; That will at least let you parse basic types without complaints.
  #-(or abcl ccl clasp ccl ecl sbcl sicl) tspec)

;;; These are also kind of messy, for about the same reason.
(defmethod string-uaets (client)
  #+abcl '(nil base-char character)
  #+ccl '(base-char) ; FIXME: verify this. What is UAET of CHARACTER?
  #+clasp '(base-char character)
  #+cmucl '(base-char) ; Also verify
  #+ecl '(base-char character)
  #+sbcl '(nil base-char character)
  #+sicl '(character)
  #-(or abcl ccl clasp cmucl ecl sbcl sicl) '(base-char character)) ; guess!

(defmethod class-aliases (client)
  #+(or abcl ccl cmucl ecl sbcl) ()
  #+clasp
  '((core:abstract-simple-vector (simple-array * (*)))
    (core:simple-vector-fixnum (simple-array fixnum (*)))
    (core:simple-vector-byte2-t (simple-array ext:byte2 (*)))
    (core:simple-vector-byte4-t (simple-array ext:byte4 (*)))
    (core:simple-vector-byte8-t (simple-array ext:byte8 (*)))
    (core:simple-vector-byte16-t (simple-array ext:byte16 (*)))
    (core:simple-vector-byte32-t (simple-array ext:byte32 (*)))
    (core:simple-vector-byte64-t (simple-array ext:byte64 (*)))
    (core:simple-vector-int2-t (simple-array ext:integer2 (*)))
    (core:simple-vector-int4-t (simple-array ext:integer4 (*)))
    (core:simple-vector-int8-t (simple-array ext:integer8 (*)))
    (core:simple-vector-int16-t (simple-array ext:integer16 (*)))
    (core:simple-vector-int32-t (simple-array ext:integer32 (*)))
    (core:simple-vector-int64-t (simple-array ext:integer64 (*)))
    (core:simple-vector-float (simple-array single-float (*)))
    (core:simple-vector-double (simple-array double-float (*)))
    (core:simple-character-string (simple-array character (*)))

    (core:complex-vector (and (not simple-array) (array * (*))))
    (core:bit-vector-ns (and (not simple-array) (array bit (*))))
    (core:complex-vector-fixnum
     (and (not simple-array) (array fixnum (*))))
    (core:complex-vector-byte2-t
     (and (not simple-array) (array ext:byte2 (*))))
    (core:complex-vector-byte4-t
     (and (not simple-array) (array ext:byte4 (*))))
    (core:complex-vector-byte8-t
     (and (not simple-array) (array ext:byte8 (*))))
    (core:complex-vector-byte16-t
     (and (not simple-array) (array ext:byte16 (*))))
    (core:complex-vector-byte32-t
     (and (not simple-array) (array ext:byte32 (*))))
    (core:complex-vector-byte64-t
     (and (not simple-array) (array ext:byte64 (*))))
    (core:complex-vector-int2-t
     (and (not simple-array) (array ext:integer2 (*))))
    (core:complex-vector-int4-t
     (and (not simple-array) (array ext:integer4 (*))))
    (core:complex-vector-int8-t
     (and (not simple-array) (array ext:integer8 (*))))
    (core:complex-vector-int16-t
     (and (not simple-array) (array ext:integer16 (*))))
    (core:complex-vector-int32-t
     (and (not simple-array) (array ext:integer32 (*))))
    (core:complex-vector-int64-t
     (and (not simple-array) (array ext:integer64 (*))))
    (core:complex-vector-float
     (and (not simple-array) (array single-float (*))))
    (core:complex-vector-double
     (and (not simple-array) (array double-float (*))))
    (core:str8ns (and (not simple-array) (array base-char (*))))
    (core:str-wns (and (not simple-array) (array character (*))))
    (core:complex-vector-t (and (not simple-array) (array t (*))))

    (core:simple-mdarray (and (not vector) simple-array))
    (core:simple-mdarray-bit (and (not vector) (simple-array bit)))
    (core:simple-mdarray-fixnum
     (and (not vector) (simple-array fixnum)))
    (core:simple-mdarray-byte2-t
     (and (not vector) (simple-array ext:byte2)))
    (core:simple-mdarray-byte4-t
     (and (not vector) (simple-array ext:byte4)))
    (core:simple-mdarray-byte8-t
     (and (not vector) (simple-array ext:byte8)))
    (core:simple-mdarray-byte16-t
     (and (not vector) (simple-array ext:byte16)))
    (core:simple-mdarray-byte32-t
     (and (not vector) (simple-array ext:byte32)))
    (core:simple-mdarray-byte64-t
     (and (not vector) (simple-array ext:byte64)))
    (core:simple-mdarray-int2-t
     (and (not vector) (simple-array ext:integer2)))
    (core:simple-mdarray-int4-t
     (and (not vector) (simple-array ext:integer4)))
    (core:simple-mdarray-int8-t
     (and (not vector) (simple-array ext:integer8)))
    (core:simple-mdarray-int16-t
     (and (not vector) (simple-array ext:integer16)))
    (core:simple-mdarray-int32-t
     (and (not vector) (simple-array ext:integer32)))
    (core:simple-mdarray-int64-t
     (and (not vector) (simple-array ext:integer64)))
    (core:simple-mdarray-float
     (and (not vector) (simple-array single-float)))
    (core:simple-mdarray-double
     (and (not vector) (simple-array double-float)))
    (core:simple-mdarray-base-char
     (and (not vector) (simple-array base-char)))
    (core:simple-mdarray-character
     (and (not vector) (simple-array character)))
    (core:simple-mdarray-t
     (and (not vector) (simple-array t)))

    (core:mdarray (and array (not vector)))
    (core:mdarray-bit
     (and (not simple-array) (not vector) (array bit)))
    (core:mdarray-fixnum
     (and (not simple-array) (not vector) (array fixnum)))
    (core:mdarray-byte2-t
     (and (not simple-array) (not vector) (array ext:byte2)))
    (core:mdarray-byte4-t
     (and (not simple-array) (not vector) (array ext:byte4)))
    (core:mdarray-byte8-t
     (and (not simple-array) (not vector) (array ext:byte8)))
    (core:mdarray-byte16-t
     (and (not simple-array) (not vector) (array ext:byte16)))
    (core:mdarray-byte32-t
     (and (not simple-array) (not vector) (array ext:byte32)))
    (core:mdarray-byte64-t
     (and (not simple-array) (not vector) (array ext:byte64)))
    (core:mdarray-int2-t
     (and (not simple-array) (not vector) (array ext:integer2)))
    (core:mdarray-int4-t
     (and (not simple-array) (not vector) (array ext:integer4)))
    (core:mdarray-int8-t
     (and (not simple-array) (not vector) (array ext:integer8)))
    (core:mdarray-int16-t
     (and (not simple-array) (not vector) (array ext:integer16)))
    (core:mdarray-int32-t
     (and (not simple-array) (not vector) (array ext:integer32)))
    (core:mdarray-int64-t
     (and (not simple-array) (not vector) (array ext:integer64)))
    (core:mdarray-float
     (and (not simple-array) (not vector) (array single-float)))
    (core:mdarray-double
     (and (not simple-array) (not vector) (array double-float)))
    (core:mdarray-base-char
     (and (not simple-array) (not vector) (array base-char)))
    (core:mdarray-character
     (and (not simple-array) (not vector) (array character)))
    (core:mdarray-t
     (and (not simple-array) (not vector) (array t))))
  #+sicl
  '((sicl-array:array-t (array t))
    (sicl-array:array-bit (array bit))
    (sicl-array:array-unsigned-byte-8 (array (unsigned-byte 8)))
    (sicl-array:array-unsigned-byte-32 (array (unsigned-byte 32)))
    (sicl-array:array-signed-byte-32 (array (signed-byte 32)))
    (sicl-array:array-unsigned-byte-64 (array (unsigned-byte 64)))
    (sicl-array:array-signed-byte-64 (array (signed-byte 64)))
    (sicl-array:array-character (array character))
    (sicl-array:array-single-float (array single-float))
    (sicl-array:array-double-float (array single-float))
    (sicl-array:array-complex-single-float (array (complex single-float)))
    (sicl-array:array-complex-double-float (array (complex double-float)))
    (sicl-array:vector-unsigned-byte-8 (vector (unsigned-byte 8)))
    (sicl-array:vector-unsigned-byte-32 (vector (unsigned-byte 32)))
    (sicl-array:vector-signed-byte-32 (vector (signed-byte 32)))
    (sicl-array:vector-unsigned-byte-64 (vector (unsigned-byte 64)))
    (sicl-array:vector-signed-byte-64 (vector (signed-byte 64))))
  #-(or abcl ccl clasp cmucl ecl sbcl sicl) ()) ; guess!
