# cl-kiviat

Small library for Kiviatdiagram generation with some limitations (e.g. fixed amount of Colors)

## Example

The function test is in the cl-kiviat file.

```cl
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

```

kiviat -> Function for diagram generation
diagram -> Create diagram data structure
  p-gruppe -> Create group of properties
     p -> Create property


## Test Image:
![Kiviat Test Image](MartinEnders.github.com/repository/cl-kiviat/kiviat-test.png)

## Development

I developed and tested cl-kiviat on
* Debian GNU/Linux (jessie)
with
* SBCL 1.1.15
and 
* SLIME