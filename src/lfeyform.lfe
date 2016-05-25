(defmodule lfeyform
  (export (render-field 2)
          (validate-fields 2)))

(include-lib "yaws/include/yaws_api.hrl")
(include-lib "exemplar/include/html-macros.lfe")
(include-file "include/fieldspecs.lfe")

;;; Render HTML fields and validate their submitted values.
;;;
;;; lfeyform supports HTML field values and HTTP GET/POST parameter
;;; values represented as Erlang primitive types--strings (Yaws uses
;;; strings rather than binaries) and booleans at this point. Erlang
;;; integers are used for some field attributes. See the type-spec
;;; comments for each function.

;; TODO: escape-string/1 as recommended by OWASP
;; (https://www.owasp.org/).
(defun escape-string (string)
  (string:strip string))

;; render-input (string string integer integer string string) -> string
(defun render-input (type name _min-length max-length placeholder value)
  (let ((type-string (atom_to_list type))
        (name-string (escape-string name))
        (max-length-string (if (=:= max-length 'undefined)
                             "-1"
                             (escape-string (integer_to_list max-length))))
        (placeholder-string (escape-string placeholder))
        (value-string (escape-string value)))
    (input `(type ,type-string
                  class "form-control"
                  id ,name-string
                  name ,name-string
                  placeholder ,placeholder-string
                  max-length ,max-length-string
                  value ,value-string))))

;; render-checkbox (string boolean) -> string
(defun render-checkbox
  ((name (= 'true _value))
   (let ((name-string (escape-string name)))
     (input `(type "checkbox"
                   id ,name-string
                   name ,name-string
                   value "on"
                   checked "checked"))))
  ((name (= 'false _value))
   (let ((name-string (escape-string name)))
     (input `(type "checkbox"
                   id ,name-string
                   name ,name-string
                   value "on")))))

;; render-field (fieldspec string|boolean) -> string
(defun render-field
  (((match-text-fieldspec name name
                          min-length min-length
                          max-length max-length
                          placeholder placeholder)
    value)
   (render-input 'text name min-length max-length placeholder value))
  (((match-password-fieldspec name name
                              min-length min-length
                              max-length max-length
                              placeholder placeholder)
    value)
   (render-input 'password name min-length max-length placeholder value))
  (((match-checkbox-fieldspec name name)
    value)
   (render-checkbox name value)))

;; validate-string ('undefined|string integer integer) -> (tuple 'ok string) | (tuple 'error atom)
(defun validate-string
  (((= 'undefined _value) _min-length _max-length)
   (tuple 'error 'undefined))
  ((value min-length max-length)
   (let ((string (string:strip value)))
     (let ((length (string:len string)))
       (cond ((< length min-length)
              (tuple 'error 'too-short))
             ((and (=/= max-length 'undefined)
                   (< max-length length))
              (tuple 'error 'too-long))
             ('true
              (tuple 'ok string)))))))

;; validate-boolean ('undefined|string) -> (tuple 'ok 'true) | (tuple 'ok 'false)
(defun validate-boolean
  (((= 'undefined _value))
   (tuple 'ok 'false))
  ((value) (when (is_list value))
   (tuple 'ok 'true)))

;; validate-field (fieldspec 'undefined|string) -> (tuple 'ok any) | (tuple 'error atom)
(defun validate-field
  (((match-text-fieldspec min-length min-length max-length max-length)
    value)
   (validate-string value min-length max-length))
  (((match-password-fieldspec min-length min-length max-length max-length)
    value)
   (validate-string value min-length max-length))
  (((match-checkbox-fieldspec)
    value)
   (validate-boolean value)))

;; fieldspec-name (fieldspec) -> string
(defun fieldspec-name
  (((match-text-fieldspec name name)) name)
  (((match-password-fieldspec name name)) name)
  (((match-checkbox-fieldspec name name)) name))

;; validate-fields (list map) -> (tuple map list)
(defun validate-fields (fieldspecs field-values)
  (let-function ((validate
                  (match-lambda
                    ((fieldspec (tuple new-field-values field-errors))
                     (let ((field-name (fieldspec-name fieldspec)))
                       (let ((field-value (maps:get field-name field-values 'undefined)))
                         (case (validate-field fieldspec field-value)
                           ((tuple 'ok valid-field-value)
                            (tuple (map-set new-field-values field-name valid-field-value)
                                   field-errors))
                           ((tuple 'error reason)
                            (tuple new-field-values
                                   (cons field-name field-errors))))))))))
    (lists:foldl #'validate/2 #(#m() ()) fieldspecs)))
