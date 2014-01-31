;;;; cl-kiviat.lisp

(in-package #:cl-kiviat)

(defvar *id* nil "ID-Pool")
(defvar *svg-doctype* nil "Doctype String fuer SVG Standalone ausgabe")
(defvar *svg-rahmen* nil  "Formatstring fuer SVG-Ausgabe mit Option auf Doctype")
(defvar *svg-breite* nil  "Breite des SVG-Bildes")
(defvar *svg-hoehe*  nil  "Hoehe des SVG-Bildes")
(defvar *svg-farben* nil  "Liste von Farbwerten die fuer die Gruppierung im Graphen verwendet werden.")


(setf *id* 0)

(setf *svg-farben* '("aqua" "blue" "brown" "burlywood" "cadetblue" "chartreuse" "cornflowerblue" "cyan" "darkseagreen" "deeppink" "firebrick"))


(setf *svg-doctype* "<?xml version=\"1.0\" standalone=\"no\"?>

<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" 
\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")


(setf *svg-rahmen* 
"~A
<svg width=\"1000\" height=\"800\" version=\"1.1\"
xmlns=\"http://www.w3.org/2000/svg\">

~A

</svg>
")

(setf *svg-breite* 1000)
(setf *svg-hoehe*  800)


(defclass diagramm ()
  ((id                :initarg :id               :initform (incf *id*) :accessor id)
   (titel             :initarg :titel            :initform ""                 :accessor titel)
   (eigenschaften     :initarg :eigenschaften    :initform nil                :accessor eigenschaften
		      :documentation "Liste von p-gruppe objekten")))

(defun diagramm (titel &rest eigenschaften)
  (make-instance 'diagramm :titel titel :eigenschaften eigenschaften))

(defmethod print-object ((obj diagramm) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (titel eigenschaften id) obj
      (format stream "Diagramm Obejekt: id: '~A' titel: '~A' eigenschaften: '~A'" id titel eigenschaften ))))

; p -> property
(defclass p ()
  ((id                :initarg :id               :initform (incf *id*) :accessor id)
   (wert              :initarg :wert             :initform 0           :accessor wert
		      :documentation "Ganzzahl die die Auspraegung der Eigenschaft im Diagramm angibt.")
   (eigenschaft       :initarg :eigenschaft      :initform ""          :accessor eigenschaft
		      :documentation "Beschreibung die im Diagramm direkt an den Achsen ausgegegebn wird.")
   (beschreibung      :initarg :beschreibung     :initform ""          :accessor beschreibung
		      :documentation "Langer Text der die Eigenschaft genauer beschreibt.")))

(defmethod print-object ((obj p) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (wert eigenschaft beschreibung id) obj
      (format stream "Property Object: id: '~A' wert: '~A' eigenschaft: '~A' beschreibung: '~A'" id wert eigenschaft beschreibung))))

(defun p (eigenschaft wert &optional (beschreibung ""))
  (make-instance 'p :wert wert :eigenschaft eigenschaft :beschreibung beschreibung))



; p-gruppe -> property-group
(defclass p-gruppe ()
  ((id                :initarg :id               :initform (incf *id*) :accessor id)
   (beschreibung      :initarg :beschreibung     :initform ""          :accessor beschreibung
		      :documentation "Text zur Gruppe von Eigenschaften.")
   (gruppe            :initarg :gruppe           :initform '()         :accessor gruppe
		      :documentation "Liste von p Objekten die zu der Gruppe gehoeren")))

(defmethod print-object ((obj p-gruppe) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (gruppe beschreibung id) obj
      (format stream "Property-Group Object: id: '~A' beschreibung: '~A' gruppe: '~A'" id beschreibung gruppe))))


(defun p-gruppe (beschreibung &rest gruppe)
  (make-instance 'p-gruppe :gruppe gruppe :beschreibung beschreibung))

(defun objekt? (objekt typ)
  (eq (type-of objekt) typ))

(defun anzahl-eigenschaften (diagramm)
  "Ermitteln der Anzahl der Eigenschaften -> Eigenschaften + Eigenschaften die in Gruppen gekapselt sind"
  (let ((anzahl 0))
    (loop for obj in (eigenschaften diagramm)
	  do (cond ((objekt? obj 'p)
		    (incf anzahl)
		    (print (eigenschaft obj)))
		   ((objekt? obj 'p-gruppe)
		    (setf anzahl (+ anzahl (length (gruppe obj)))))))
    anzahl))

(defun eigenschaften-in-liste (diagramm)
  "Sammeln aller Eigenschaften aus dem Diagramm in eine ein-ebenen-Liste"
  (let ((flache-liste nil))
    (loop for obj in (eigenschaften diagramm)
          do (cond ((objekt? obj 'p)
		    (push obj flache-liste))
		   ((objekt? obj 'p-gruppe)
		    (loop for sub-obj in (gruppe obj)
			  do (push sub-obj flache-liste)))))
    (reverse flache-liste)))
        




(defun x-kart (winkel radius &key (offset 0))
  (round (+ (* radius (cos winkel)) offset)))

(defun y-kart (winkel radius &key (offset 0))
  (round (+ (* radius (sin winkel)) offset)))

(defun zentrum (laenge)
  (/ laenge 2))

(defun line (x1 y1 x2 y2 &key (stream nil))
  (format stream "<line x1=\"~A\" y1=\"~A\" x2=\"~A\" y2=\"~A\"/>" x1 y1 x2 y2))

(defun polygon (punktliste &key (stream nil) (fill "none" fill-supplied-p))
  "Erwartet fuer punktliste ein Liste von Cons mit (x . y)"
  (format stream "<polygon ~A points=\"~A\" />" (if fill-supplied-p (format nil "fill=\"~A\"" fill) "")
	                                        (with-output-to-string (s)
						  (dolist (x punktliste)
						    (format s "~A,~A " (car x) (cdr x))))))

(defun text (text x y &key (stream nil) (font-family "Verdana") (font-size 12) (fill "green") (text-anchor "start"))
  (format stream "<text x=\"~A\" y=\"~A\" text-anchor=\"~A\" font-family=\"~A\" font-size=\"~A\" fill=\"~A\" >~A</text>" 
	  x y text-anchor font-family font-size fill text))

(defun text-position (winkel radius)
  "Multible Rueckgabewerte: x-Koordinate, y-Koordinate, text-anchor fuer SVG (start | middle | end)"
  (let* ((x-zentrum (zentrum *svg-breite*))
	 (y-zentrum (zentrum *svg-hoehe*))
	 (x (x-kart winkel radius :offset x-zentrum))
	 (y (y-kart winkel radius :offset y-zentrum))
	 (ausrichtung "start")
	 (toleranz 4))
    (flet ((x-mitte-p () (and (> x (- x-zentrum toleranz))
			      (< x (+ x-zentrum toleranz))))
	   (x-links-p () (< x x-zentrum))
	   (y-mitte-p () (and (> y (- y-zentrum toleranz))
			      (< y (+ y-zentrum toleranz))))
	   (y-oben-p () (< y y-zentrum)))

      (setf ausrichtung (cond ((x-mitte-p) "middle")
			      ((x-links-p) "end")
			      (t "start")))

      (setf x (cond ((x-mitte-p) x)
		    ((x-links-p) (- x 4))
		    (t (+ x 4))))

      (setf y (cond ((y-mitte-p) y)
		    ((y-oben-p) (- y 7))
		    (t (+ y 14))))
      
      (values x y ausrichtung))))
  
  

(defun render-diagramm (diagramm &key (stream nil))
  (format stream "~A"
	  (with-output-to-string (s)
	    (format s "<div><p>Textversion des Diagramms:</p>~%")
	    (format s "<p><b>~A</b></p>~%" (titel diagramm))
	    (format s "<ul>~%")
	    (dolist (gruppe (eigenschaften diagramm))
	      (format s "<li>[~A] ~A</li>~%" (id gruppe) (beschreibung gruppe))
	      (format s "<ul>~%")
	      (dolist (p (gruppe gruppe))
		(format s "<li>[~A] - ~A: <span style='color: red;'>~A</span></li>~%
<div id='~A' style='position: relative'>
<div style='position: absolute; left: ~Apx; width: 10px; height: 20px; background-color: red; display: inline-block; margin: 0; padding: 0;'></div>
<div style='width: 320px; height: 6px; background-color: blue; display: inline-block; margin: 0; padding: 0;'></div>
</div>" (id p) (eigenschaft p) (wert p) (id p) (* 80 (wert p))))
	      (format s "</ul>~%"))
	    (format s "</ul>~%")
	    (format s "</div>~%"))))


(defun grundgeruest (&key diagramm (anzahl-raster 4) (stern-laenge 300) (stream nil))
  (format stream "~A" 
  (with-output-to-string (s)
    (flet ((start-winkel () (* (/ 3 2) pi)))
      (flet ((winkel-berechnung (i &key (start-winkel (start-winkel))
				   (winkel (/ (* 2 pi) (anzahl-eigenschaften diagramm))))
	       (+ start-winkel (* winkel (1- i))))
	     (raster-radius (i) (* (/ stern-laenge anzahl-raster) i)))


	;; Titel
	(format s "~A~%" (text (titel diagramm) (/ *svg-breite* 2) 30 :text-anchor "middle" :fill "black" :font-size 25))
	
	;; Zeichnen der Achsen
	(format s "<g stroke=\"green\" stroke-width=\"1px\">~%")
	(loop for i from 1 to (anzahl-eigenschaften diagramm)
	   do (format s "~A~%" (line (zentrum *svg-breite*) 
				     (zentrum *svg-hoehe*) 
				     (x-kart (winkel-berechnung i) stern-laenge :offset (zentrum *svg-breite*))
				     (y-kart (winkel-berechnung i) stern-laenge :offset (zentrum *svg-hoehe*)))))
	
	;; Zeichnen der Polygone fuer ablesen der Bewertungsraster
	(format s "<g fill=\"none\">~%")
	(dolist (polygon (loop for n from 1 to anzahl-raster
			    collect (loop for i from 1 to (anzahl-eigenschaften diagramm)
				       collect (cons (x-kart (winkel-berechnung i) (raster-radius n) :offset (zentrum *svg-breite*))
						     (y-kart (winkel-berechnung i) (raster-radius n) :offset (zentrum *svg-hoehe*))))))
	  (format s "~A~%" (polygon polygon)))
	(format s "</g>~%")
	
	;; Beschriftung der Bewertungsraster
	(loop for n from 1 to anzahl-raster
	   do (format s "~A~%" (text n 
				     (x-kart (start-winkel) (raster-radius n) :offset (+ (zentrum *svg-breite*) 3) )
				     (y-kart (start-winkel) (raster-radius n) :offset (+ (zentrum *svg-hoehe*)  17) ))))

	(format s "</g>~%")



	;; zeichnen der gesamten challenge
	(format s "<g stroke=\"grey\" fill-opacity=\"0.4\" fill=\"lightgrey\">~%")
	(format s "~A~%" (polygon (loop for i in (eigenschaften-in-liste diagramm) ; Durchlaufen der einzelnen Eigenschaften
				     for n from 1 to (anzahl-eigenschaften diagramm) ; Zaehler fuer Rotation der der Punkte um Zentrum
				     collect (cons (x-kart (winkel-berechnung n) (raster-radius (min (wert i) anzahl-raster)) :offset (zentrum *svg-breite*))
						   (y-kart (winkel-berechnung n) (raster-radius (min (wert i) anzahl-raster)) :offset (zentrum *svg-hoehe*))))))
	(format s "</g>~%")


	;; Zeichnen der Eigenschaftswertegruppen


	  (let (;(gesamt-polygon nil)
		(achsencounter 0))
	    (loop for ebene-1 in (eigenschaften diagramm)  ; loop ueber p-gruppen
		  for n from 1 to (length (eigenschaften diagramm))
	      do
		 (format s "<g stroke=\"~A\" fill-opacity=\"0.5\" fill=\"~A\">~%" (nth n *svg-farben*) (nth n *svg-farben*))
		 (format s "~A~%"
			 (polygon (let ((polygon-punkte (list (cons (zentrum *svg-breite*) (zentrum *svg-hoehe*)))))
				    (dolist (ebene-2 (gruppe ebene-1))
				      (incf achsencounter)
				      (push (cons (x-kart (winkel-berechnung achsencounter) 
							  (raster-radius (min (wert ebene-2) anzahl-raster)) 
							  :offset (zentrum *svg-breite*))
						  (y-kart (winkel-berechnung achsencounter) 
							  (raster-radius (min (wert ebene-2) anzahl-raster)) 
							  :offset (zentrum *svg-hoehe*)))
					    polygon-punkte))
				    (reverse polygon-punkte))))
		 (format s "</g>~%")))

	  ;;; Beschreibungen der Ebene-2 Polygone
	  (format s "~A~%" (text "All Challanges" 10 (- *svg-hoehe*  10) :text-anchor "left" :fill "grey"))
	  (let* ((anzahl-challenges (length (eigenschaften diagramm)))
		 (hoehe (- *svg-hoehe* 25 (* anzahl-challenges 15))))
	    (loop for ebene-1 in (eigenschaften diagramm)
		  for n from 1 upto anzahl-challenges
	       do (format s "~A~%" (text (beschreibung ebene-1) 10 (setf hoehe (+ hoehe 15)) :text-anchor "left" :fill (nth n *svg-farben*)))))



	  

	  (loop for i in (eigenschaften-in-liste diagramm)
	        for n from 1 to (anzahl-eigenschaften diagramm)
	        do (multiple-value-bind (x y text-ausrichtung) (text-position (winkel-berechnung n) (raster-radius anzahl-raster))
		     (format s "~A~%" (text (eigenschaft i) x y :text-anchor text-ausrichtung :fill "black")))))))))

	  





(defun kiviat (&key diagramm (anzahl-raster 4) (stern-laenge 300) (stream nil) (doctype nil))
  (format stream *svg-rahmen*
 	  (if doctype *svg-doctype* "")
	  (with-output-to-string (s)
	    (grundgeruest :diagramm diagramm :stream s :anzahl-raster anzahl-raster :stern-laenge stern-laenge))))
	    
;;;;;;;;;;; TEST

(defun test (&key (file #P"~/kiviat-test.svg"))
  (with-open-file (s file :direction :output :if-exists :supersede)
    (kiviat :diagramm (diagramm "CL-Kiviat TEST" 
				(p-gruppe "Common LISP" 
					  (p "Fun" 5)
					  (p "Power" 5)
					  (p "Taste" 1.7)
					  (p "More Fun" 2))
				(p-gruppe "Pork"
					  (p "Taste" 4)
					  (p "Price" 2.8))
				(p-gruppe "Car"
					  (p "Colors" 2)
					  (p "Wheels" 4)
					  (p "Engine" 3.5)
					  (p "Trunk" 3))
				(p-gruppe "Banana"
					  (p "Yellow" 1.8)
					  (p "Curved" 1)))
	    :anzahl-raster 5
	    :stern-laenge 300 
	    :stream s)))
