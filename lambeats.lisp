(defpackage :lambeats
  (:use :cl)
  (:export run))

(in-package :lambeats)

(defvar fill-size 10)
(defvar host "localhost")
(defvar port 6600)
(defvar refresh-time 120) ;minutes to sleep between refreshes.
(defvar category-decision-exp)
(defvar categories nil)
(defvar current-category)
(defvar current-category-songs)
(defvar connection (mpd:connect :host host :port port))

(defun update-connection ()
  (setf connection (mpd:connect :host host :port port)))

(defun random-elts (list count)
  (let ((remaining-elts (length list))
	(return-elts ()))
    (dolist (elt list)
      (when (< (random remaining-elts) count)
	(push elt return-elts)
	(decf count))
      (decf remaining-elts))
    return-elts))

(defgeneric parse-category-def (definition))
(defmethod parse-category-def ((definition string))
  definition)

(defmethod parse-category-def ((definition symbol))
  (rest (assoc definition categories)))

(defmethod parse-category-def ((definition list))
  (if (symbolp (first definition))
      (cond ((eql :and (first definition)) (apply #'intersection (append (loop for term in (rest definition) collecting (parse-category-def term)) (list :test #'equal))))
	    ((eql :or (first definition)) (apply #'union (append (loop for term in (rest definition) collecting (parse-category-def term)) (list :test #'equal))))
	    ((eql :except (first definition)) (apply #'set-difference (append (loop for term in (rest definition) collecting (parse-category-def term)) (list :test #'equal)))))
    (loop for term in definition collecting (parse-category-def term))))

(defun use-category-def (definition)
  (push (cons (first definition) (parse-category-def (second definition))) categories))


(defun update-categories (category-defs)
  (setf categories ())
  (dolist (def category-defs)
    (use-category-def def)))

(defun use-category (category-name)
  (let ((category (assoc category-name categories)))
    (setf current-category category-name)
    (setf current-category-songs nil) ;we assemble a list of all the songs in the current category so we don't have to do it on every refresh.
    (loop for dir in (rest category) do
	  
	  (dolist (song (mpd:list-all connection dir))
          
	    (when (cl-ppcre:scan "(.*)?\\.(flac|mp3|ogg)" song) ;you better not have any directories wíth music file extensions. TODO: check whether returned file is a song or directory in a non-stupid way.
	      (push song current-category-songs))))))

(defun update-current-category-* (exp)
  (dolist (option exp)
    (when (eval (first option))
      (use-category (second option))
      (return-from update-current-category-*))))

(defun update-current-category ()
  (update-current-category-* category-decision-exp))

(defun update-globals (exp)
  (dolist (global exp)
    (cond ((eql (first global) 'fill-size) (setf fill-size (second global)))
	  ((eql (first global) 'host) (setf host (second global)))
	  ((eql (first global) 'port) (setf port (second global)))
	  ((eql (first global) 'refresh-time) (setf refresh-time (second global))))))

(defun update-config (&optional (file "lambeats.conf"))
  (let ((in (open file)))
    (loop for expr = (read in nil)
          while expr do
          (cond ((eql (first expr) :globals) (update-globals (rest expr)))
                ((eql (first expr) :categories) (update-categories (rest expr)))
                ((eql (first expr) :category-picks) (setf category-decision-exp (rest expr)))))))
	  
(defun refresh-playlist ()
  (let ((songs-to-fill (- fill-size (length (mpd:playlist-info connection)))))
    (print songs-to-fill)
    (dolist (song (random-elts current-category-songs songs-to-fill))
      (print song)
      (mpd:add connection song))))

(defun run ()
  (loop
   (print "updated connection")
   (update-config)
   (print "updated config")
   (update-current-category)
   (print "updated category")
   (refresh-playlist)
   (print "refreshed playlist, entering sleep mode")
   (sleep refresh-time)
   (print "exiting sleep, refreshring")))

