(script-fu-register
  "script-fu-polychain1"
  "Polychain 1"
  "Draws an arc of triangles from the bottom-left corner."
  "John King"
  ""
  "August 2015"
  ""
  SF-ADJUSTMENT     "Width"                 '(640 1 99999 10 100 0 1)
  SF-ADJUSTMENT     "Height"                '(480 1 99999 10 100 0 1)
  SF-ADJUSTMENT     "Polygon Count"         '(100 1 99999 1  10  0 1)
  SF-ADJUSTMENT     "Rotation(times pi)"    '(0 0 100 0.5 1 10 1)
  SF-COLOR          "Start Color"           '(0 0 0)
  SF-COLOR          "End Color"             '(255 255 255)
  SF-COLOR          "Background Color"      '(0 0 255))
(script-fu-menu-register "script-fu-polychain1"
 "<Image>/File/Create/Hardcoded Pattern")
(define (script-fu-polychain1 width height polygons rotation startC endC bgC)
    (let* ((points (interval-list 0 1 polygons))
           (image (car (gimp-image-new width height RGB)))
           (drawable (car
            (gimp-layer-new image width height RGB-IMAGE "layer 1" 100 NORMAL)))
           (sequence-params
            (polychain1 width height rotation startC endC bgC points)))
        (gimp-image-add-layer image drawable 0)
        (gimp-context-set-background bgC)
        (gimp-drawable-fill drawable BACKGROUND-FILL)
        (gimp-context-set-brush "1. Pixel")
        (gimp-context-set-brush-size 1)
        (polygon-sequence image drawable sequence-params)
        (gimp-display-new image)))
(define (polychain1 width height rotation startC endC bgC points)
    (if (null? points)
        '()
        (let* ((point (car points))
                (x (* (/ 2 3) width point))
                (y (- height (* (/ 2 3) height point point)))
                (r (/ width (* point 10)))
                (vertices 3)
                (thetaCoeff (* point rotation))
                (outlineC (mixcolors startC endC point))
                (fillC bgC))
            (cons
             (list x y r vertices thetaCoeff outlineC fillC)
             (polychain1 width height rotation startC endC bgC (cdr points))))))
(define (interval-list start end len)
    (if (zero? len)
        '()
        (let ((next (+ start (/ (- end start) len))))
            (cons next (interval-list next end (- len 1))))))
