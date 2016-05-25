(defrecord text-fieldspec
  name
  (min-length 0)
  max-length
  (placeholder ""))

(defrecord password-fieldspec
  name
  (min-length 0)
  max-length
  (placeholder ""))

(defrecord checkbox-fieldspec
  name)
