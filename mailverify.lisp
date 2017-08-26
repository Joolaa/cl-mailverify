(defpackage :mailverify
  (:use #:common-lisp
        #:caveman2
        #:clack
        #:cl-ppcre
        #:dragons
        #:alexandria))

(in-package :mailverify)

(defparameter *web* (make-instance 'caveman2:<app>))

(defvar *server-instance* nil)

(defun server-down ()
  (prog1 (clack:stop *server-instance*)
    (setf *server-instance* nil)))

(defun server-up ()
  (if (not (null *server-instance*))
      (prog1 (server-down)
             (server-up))
      (setf *server-instance*
            (clack:clackup *web* :port 3000))))

(defvar *email-regex* "^([a-zA-Z0-9_.+-]+)@([a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+)$")

(defun verify-email-format (email)
  "If valid email, result is the domain part and the local part.
   If not valid, result is nil."
  (register-groups-bind (local-part domain)
      (*email-regex* email)
    (values domain local-part)))

(defun query-records (address symbol)
  (remove-if #'null (query (question address symbol))))

(defun exchange-addresses (mx-records)
  (mapcar #'(lambda (rec) (getf (rr-rdata rec) :exchange)) mx-records))

(defun contains-non-nilp (record-list)
  (not (null (remove-if #'null record-list))))

(defun default-if-nil (arg func def-value)
  (let ((result (funcall func arg)))
    (if (null result)
        def-value
        result)))

(defmacro compose-with-defaults (func-defaults arg)
  `(apply-funcs-defaults ,(cons 'list (reverse (mapcar #'(lambda (pair) (cons 'list pair)) func-defaults))) ,arg))

(defun apply-funcs-defaults (func-defaults arg)
  (if (null func-defaults)
      arg
      (let ((func (caar func-defaults))
            (def-value (cadar func-defaults)))
        (let ((result (funcall func arg)))
          (if (null result)
              def-value
              (apply-funcs-defaults (cdr func-defaults) result))))))

(defun get-validity (address)
  (flet ((get-exchanges (records) (mapcar (lambda (rec)
                                            (getf (rr-rdata rec) :exchange)) records))
         (get-a-records (addrs) (mapcar (lambda (addr)
                                          (query-records addr :a)) addrs)))
    (compose-with-defaults (((lambda (mx-records)
                               (get-a-records (get-exchanges mx-records)))
                             'no-properly-setup-servers)
                            ((lambda (address) (query-records address :mx)) 'no-mx-records)
                            (#'verify-email-format 'invalid-address)) address)))

(defun check-domain (address)
  (let ((address-validity (get-validity address)))
    (if (listp address-validity)
        'domain-properly-setup
        address-validity)))

(defun wrap-to-default (func def-value)
  (rcurry #'default-if-nil func def-value))

(caveman2:defroute "/*" ()
  (setf (getf (response-headers *response*) :content-type) "text/sexp; charset=utf-8")
  (next-route))

(caveman2:defroute ("/asd" :method :GET) (&key |name|)
  (write-to-string (verify-email-format |name|)))

(defvar *x* (query (question "alt3.gmail-smtp-in.l.google.com" :a)))

;(mapcar #'(lambda (rec) (getf (rr-rdata rec) :exchange)) (query-mx-records (verify-email-format "dlsf.kjdfs@hotmail.com")))
