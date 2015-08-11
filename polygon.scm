(script-fu-register
  "script-fu-polygon"
  "Polygon"
  "Creates a n-sided polygon and selected brush."
  "John King"
  ""
  "August 2015"
  "RGB*"
  SF-IMAGE          "Image"                 0
  SF-DRAWABLE       "Drawable"              0
  SF-ADJUSTMENT     "Center X Coordinate"   '(0 0 99999 10 100 0 1)
  SF-ADJUSTMENT     "Center Y Coordinate"   '(0 0 99999 10 100 0 1)
  SF-ADJUSTMENT     "Radius"                '(100 1 99999 10 100 0 1)
  SF-ADJUSTMENT     "Vertices"              '(3 3 99999 1 1 0 1)
  SF-ADJUSTMENT     "Rotation(times pi)"    '(0 0 2 0.125 0.5 10 1)
  SF-COLOR          "Outline Color"         '(0 0 0);black
  SF-COLOR          "Fill Color"            '(255 255 255));white
(script-fu-menu-register "script-fu-polygon" "<Image>/Image")
(define pi 3.1415926535897932384626433832795028841971693993751058)
(define
 (script-fu-polygon image drawable x y r vertices thetaCoeff outlineC fillC)
    (let* ((theta (* (- thetaCoeff (* 2 (floor (/ thetaCoeff 2)))) pi))
           (thetas (polygon-thetas vertices theta 0))
           (coords (list->vector (polygon-coords x y r vertices thetas)))
           (old-fg (car (gimp-context-get-foreground)))
           (old-bg (car (gimp-context-get-background))))
        ;start pushing to undo stack
        (gimp-undo-push-group-start image)
        ;set colors
        (gimp-context-set-foreground outlineC)
        (gimp-context-set-background fillC)
        ;fill polygon
        (gimp-image-select-polygon
            image CHANNEL-OP-REPLACE (* vertices 2) coords)
        (gimp-edit-bucket-fill
            drawable BG-BUCKET-FILL NORMAL-MODE 100 255 FALSE x y)
        (gimp-selection-none image)
        ;draw polygon outline
        (gimp-pencil drawable (* 2 (+ vertices 1)) coords)
        ;set colors back
        (gimp-context-set-foreground old-fg)
        (gimp-context-set-background old-bg)
        ;stop pushing to undo stack
        (gimp-undo-push-group-end image)
        ;display the updated image
        (gimp-displays-flush)
        ;return the following
        (list image drawable)))
(define (polygon-coords x y r vertices thetas)
    (if (null? thetas)
        '()
        (let ((theta (car thetas)))
            (append
                (list (+ x (* r (cos theta))) (+ y (* r (sin theta))))
                (polygon-coords x y r vertices (cdr thetas))))))

(define (polygon-thetas vertices theta vertex)
    (if (> vertex vertices)
        '()
        (let* ((newTheta (+ theta (/ (* 2 pi) vertices))))
            (cons theta (polygon-thetas vertices newTheta (+ vertex 1))))))
(define (polygon-sequence image drawable polygons)
    (if (null? polygons)
        '()
        (let* ((polygon (car polygons))
               (call (append (list image drawable) polygon)))
            (apply script-fu-polygon call)
            (polygon-sequence image drawable (cdr polygons)))))
