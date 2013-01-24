(in-package :asdf)

(defsystem "lambeats"
  :description "Lambeats (lambda-beats), an experimental automatic client for MPD"
  :version "0.0.1"
  :author "Eigil Rischel <ayegill@gmail.com>"
  :components ((:file "lambeats"))
  :depends-on (:mpd :cl-ppcre))