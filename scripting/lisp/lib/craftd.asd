(defpackage #:craftd
  (:use #:cl #:asdf))

(in-package :craftd)

(unless (find :unicode *features*)
  (error "Please build ECL with unicode support (configure --enable-unicode)"))

(unless (find :threads *features*)
  (error "Please build ECL with thread support (configure --enable-threads)"))

(defsystem :craftd
  :version "0.1"

  :serial t

  :components (
    (:file "string")
    (:file "minecraft")
    (:file "wrap")
    (:file "dispatcher")
    (:file "server")
    (:file "client")
    (:file "player")))
