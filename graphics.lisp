(ql:quickload "clx")
(ql:quickload "zpng")
(ql:quickload "png-read")

(defparameter *test-image-path* "./test.png")
(defparameter *default-width* 400)
(defparameter *default-height* 300)
(defparameter *default-array* (zpng:data-array (make-instance 'zpng:png
                                                              :color-type :truecolor-alpha
                                                              :width *default-width*
                                                              :height *default-height*)))


(defparameter *expected-truecolor-alpha-error*
  "WRONG IMAGE FORMAT: expected truecolor-alpha")

(defparameter *coordinates-error*
  "WRONG COORDINATES: coordinates outside the array")

(defparameter *too-long-axes-error*
  "TOO-LONG-AXES: axes length is too big!")

;; Печатает сообщение об ошибке
;; Принимает имя функции, в которой произошла ошибка,  и строку, которую надо вывести
(defun print-error (func-name string)
  (format t "~A: ~A ~%" func-name string))


;; Макрос позволяет записать в пиксель, расположенный в массиве
;; по координатам X и Y значение RGB. Эта операция будет часто повторяться,
;; поэтому вынесена в макрос.
(defmacro write-to-array (array x y r g b)
  `(setf (aref ,array ,y ,x 0) ,r
         (aref ,array ,y ,x 1) ,g
         (aref ,array ,y ,x 2) ,b
         (aref ,array ,y ,x 3) 255))

;; (macroexpand-1 '(write-to-array *default-array* 1 2 255 0 0))

;; заполняет массив фоновым цветом
(defun write-background-color-trucolor (array r g b)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    (if (= format 4)
        (do ((x 0 (incf x)))
            ((= x width))
          (do ((y 0 (incf y)))
              ((= y hight))
            (write-to-array array x y r g b)))
        (print-error 'write-background-color-trucolor
                     *expected-truecolor-alpha-error*))))


;; (write-background-color-trucolor *default-array* 0 255 0 )
;; (save-png (array-dimension *default-array* 0)
;;           (array-dimension *default-array* 1)
;;           "./test.png"
;;           *default-array*)

(defun write-straight-line (array x0 y0 x1 y1 r g b)
  ;; (format t "write-straight-line rgb ~A ~A ~A ~%" r g b)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    (if (or (<= y1 hight)
            (>= x1 width))
        (print-error 'write-straight-line
                     *coordinates-error*)
        (if (not (= format 4))
            (print-error 'write-straight-line
                         *expected-truecolor-alpha-error*)
            (if (= x0 x1)
                (do ((y y0 (decf y)))
                    ((= y y1))
                  (write-to-array array x0 y r g b))
                (do ((x x0 (incf x)))
                    ((= x x1))
                  (write-to-array array x y0 r g b)))))))

;; Отрисовывает оси X и Y
;; PARAMS
;; - массив изображения
;; - длина осей x и y в пикселях
;; - цвет в формате RGB
(defun write-axises (array x-axis-length y-axis-length r g b)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    ;; чтоб не рисовать от края изображения, высчитываем отступ
    ;; в 10% от границы изображения
    (let* ((x0 (* ( floor width 100) 10))
           (y0 (* ( floor hight 100) 90))
           (x1 (+ x0 x-axis-length))
           (y1 (- y0 y-axis-length)))
      (if (or (< y1 0) (> x1 width))
          (print-error 'write-axises *too-long-axes-error*)
          (if (not (= format 4))
              (print-error
               'write-axises *expected-truecolor-alpha-error*)
              (progn
                ;; отрисовываем ось Y
                (write-straight-line array x0 y0 x0 y1 r g b)
                ;; отрисовываем ось X
                (write-straight-line array x0 y0 x1 y0 r g b)))))))

;; Рисует заданное кол-во делений на осях, самостоятельно высчитывая размер
;; делений и рассояние между ними
;; # PARAMS: #
;; - массив изображения
;; - длина осей в пикселях
;; - цвет для делений (задается RGB)
;; - кол-во делений
;; # RETURN VALUE: #
;; возвращает список, состоящий из 2х подсписков
;; первый подсписок содержит в себе список координат, отображающих делений на оси X
;; второй подсписок содержит в себе аналогичный список для делений по оси Y
(defun write-divs-on-axises (array x-axis-length y-axis-length r g b divs)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    (let* ((start-x (* ( floor width 100) 10))
           (start-y (* ( floor hight 100) 90))
           (divs-y-pixels (floor y-axis-length divs))
           (divs-x-pixels (floor x-axis-length divs))
           (divs-size (* ( floor width 100) 2))
           (y-divs-list '())
           (x-divs-list '()))
      ;; (format t "divs-size ~A ~%" divs-size)
      (if (or (< (- start-y y-axis-length) 0)
              (> (+ start-x x-axis-length) width))
          (print-error 'write-divs-on-axises *too-long-axes-error*)
          (if (= format 4)
              (progn
                ;; двигаемся по оси x слева направо
                ;; рисуя еделния через каждые n пикселей
                (do ((x start-x (+ x divs-x-pixels)))
                    ((>= x (+ start-x x-axis-length)))
                  ;; рисуем деления длины n на оси x
                  ;; координата x не меняяется
                  (setf x-divs-list (append x-divs-list
                                            (list (cons x start-y))))
                  (do ((y start-y (incf y)))
                      ((= y (+ start-y divs-size)))
                    (write-to-array array x y r g b)))

                ;; двигаемся по оси Y, двигаясь сверху вниз
                ;; и рисуя через каждые n пикселейни деления
                (do ((y start-y (- y divs-y-pixels)))
                    ((<= y (- start-y y-axis-length)))
                  ;; рисуем деления длины n на оси y
                  ;; координата Y не меняется
                  (setf y-divs-list (append y-divs-list
                                            (list (cons start-x y))))
                  (do ((x start-x (decf x)))
                      ((= x (- start-x divs-size)))
                    (write-to-array array x y r g b)))
                (values x-divs-list y-divs-list))
              (print-error 'write-divs-on-axises *expected-truecolor-alpha-error*))))))

;; (defun write-divs-on-axises-test()
;;   (write-background-color-trucolor *default-array* 255 255 255)
;;   (write-axises *default-array* 110 110 54 98 23)
;;   (write-divs-on-axises *default-array* 110 110 200 13 20 10)
;;   (save-png (array-dimension *default-array* 0)
;;             (array-dimension *default-array* 1)
;;             "./test.png"
;;             *default-array*))

;; (write-divs-on-axises-test)

;; Отрисовывает точки на графе
;; # PARAMS #
;; - массив изображения
;; - ассоциативный (!) список координат, где car подсписка - координата X
;; - цвет в формате RGB, которым отрисовываются точки
(defun write-points (array points-list r g b)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    (if (< format 4)
        (print-error 'write-points *expected-truecolor-alpha-error*)
        (write-points-rec array points-list hight width r g b))))

(defun write-points-rec (array points-list hight width r g b)
  (if (null points-list)
      'done
      (let ((x (caar points-list))
            (y (cdar points-list)))
        (if (or (>= x width)
                (>= y hight))
            (print-error 'write-points-rec *coordinates-error*)
            (progn
              (write-to-array array x y r g b)
              (write-points-rec array
                                (cdr points-list) hight width r g b))))))


;; (defun test-write-points-rec()
;;   (write-background-color-trucolor *default-array* 255 255 255)
;;   (multiple-value-bind (l1 l2)
;;       (write-divs-on-axises *default-array* 110 110 255 255 255 10)
;;     (write-points *default-array* l1 0 0 0 )
;;     (write-points *default-array* l2 0 0 0 ))
;;   (save-png (array-dimension *default-array* 0)
;;             (array-dimension *default-array* 1)
;;             "./test.png"
;;             *default-array*))
;; (test-write-points-rec)


;; Усовершенствованная реализация алгоритма Брезенхайма - позволяет соединять 2 точки,
;; если:
;; - они лежат на одной прямой
;; - точка А находится ближке к точке 0.0, чем точка В (т.е. график растет из ниженго
;; левого угла в правый верхний
;; - точка В находится выше по оси Y точки А и при этом точка А  находится дальше по
;; оси X (т.е. график нисходящий из левого верхнего угла в правый нижний)
(defun bresenham-algorithm (array x0 y0 x1 y1 r g b)
  (bresenham-algorithm-rec array x0 y0 x1 y1 0 r g b))

(defun bresenham-algorithm-rec (array x0 y0 x1 y1 error-rate r g b)
  (format t "bresenham-algorithm: x0 ~A y0 ~A x1 ~A y1 ~A ~%" x0 y0 x1 y1)
  ;; достигли искомой точки
  (cond ((and (= x0 x1) (= y0 y1)) 'done)
        ;; точки лежат на одной прямой по оси Y
        (( = x0 x1) (if ( > y0 y1)
                        (progn
                          (write-straight-line array x0 y0 x1 y1 r g b)
                          'done)
                        (progn
                          (write-straight-line array x0 y1 x1 y0 r g b)
                          'done)))
        ;; точки лежат на одной прямой по оси X
        (( = y0 y1) (if ( > x0 x1)
                        (progn
                          (write-straight-line array x1 y0 x0 y1 r g b)
                          'done)
                        (progn
                          (write-straight-line array x0 y0 x1 y1 r g b)
                          'done)))
        (t
         ;; точки не лежат на одной прямой
         (let* ((angle (* (/ (- y1 y0) (- x1 x0)) 1.0))
                (new-error-rate (+ error-rate angle)))
           ;; (format t "angle ~A ~%" angle)
           (write-to-array array x0 y0 r g b)
           ;; угол положительный?
           (if ( >= new-error-rate 0)
               (if (<= new-error-rate 0.5)
                   (bresenham-algorithm-rec array (incf x0) y0 x1 y1 new-error-rate r g b)
                   (bresenham-algorithm-rec array (incf x0) (incf y0) x1 y1
                                            (- new-error-rate 1) r g b))
               ;; угол отрицательный (т.е. рисуем нисходящий график)
               (if (>= new-error-rate -0.5)
                   (bresenham-algorithm-rec array (decf x0) y0 x1 y1 new-error-rate r g b)
                   (bresenham-algorithm-rec array (decf x0) (incf y0) x1 y1
                                            (+ new-error-rate 1) r g b)))))))

;; (defun test-bresenham-algorithm()
;;   (write-background-color-trucolor *default-array* 255 255 255)
;;   (bresenham-algorithm *default-array* 10 3 2 10 255 0 0)
;;   (save-png (array-dimension *default-array* 1)
;;             (array-dimension *default-array* 0)
;;             "./test.png"
;;             *default-array*))
;; (test-bresenham-algorithm)

;; Рисует ЛЮБУЮ линию от точки А до точки В
(defun write-any-line (array x0 y0 x1 y1 r g b)
  ;; (format t "write-any-line: x0 ~A x1 ~A ~%" x0 x1)
  (if (or (= x0 x1)
          (= y0 y1))
      ;; точки лежат на одной прямой
      (write-straight-line array x0 y0 x1 y1 r g b)
      ;; точки НЕ лежат на одной прямой
      (if (< y1 y0)
          (bresenham-algorithm array x1 y1 x0 y0 r g b)
          (bresenham-algorithm array x0 y0 x1 y1 r g b))))


(defun generate-graphic (array list-of-points r g b)
  (destructuring-bind (hight width format)
      (array-dimensions array)
    (if (not (= format 4))
        (print-error 'generate-graphic
                     *expected-truecolor-alpha-error*)
        (let* ((x-axis-length (* (/ width 100) 80))
               (y-axis-length (* (/ hight 100) 80))
               (y-axis-end-point (* ( floor hight 100) 90))
               (x-axis-start-point (* ( floor width 100) 10)))
          (write-background-color-trucolor array 255 255 255)
          (write-axises array x-axis-length y-axis-length 0 0 0)
          (write-divs-on-axises array x-axis-length y-axis-length 0 0 0
                                (length list-of-points))
          (generate-graphic-rec array list-of-points y-axis-end-point
                                x-axis-start-point r g b)))))

(defun generate-graphic-rec (array list-of-points y-axis-end-point x-axis-start-point
                             r g b)
  ;; (format t "y-axis-end-point ~A~%" y-axis-end-point)
  (if (= (length list-of-points) 1)
      'done
      (let* ((point1 (car list-of-points))
             (point2 (cadr list-of-points))
             (x1 (+ x-axis-start-point (car point1)))
             (y1 (- y-axis-end-point (cdr point1)))
             (x2 (+ (car point2) x-axis-start-point))
             (y2 (- y-axis-end-point (cdr point2))))
        ;; (format t "y1 ~A y2 ~A~%" y1 y2)
        (write-any-line array x1 y1 x2 y2 r g b)
        (generate-graphic-rec array (cdr list-of-points) y-axis-end-point
                              x-axis-start-point r g b))))


;; сохраняет массив в виде изображения
(defun save-png (width height pathname-str image
                 &optional (color-type :truecolor-alpha))
  (let* ((png (make-instance 'zpng:png :width width :height height
                             :color-type color-type))
         (vector (make-array ;; displaced vector - need copy for save
                  (* height width (zpng:samples-per-pixel png))
                  :displaced-to image :element-type '(unsigned-byte 8))))
    ;; Тут применен потенциально опасный трюк, когда мы создаем
    ;; объект PNG без данных, а потом добавляем в него данные,
    ;; используя неэкспортируемый writer.
    ;; Это нужно чтобы получить третью размерность массива,
    ;; который мы хотим передать как данные и при этом
    ;; избежать создания для этого временного объекта
    (setf (zpng::%image-data png) (copy-seq vector))
    (zpng:write-png png pathname-str)))

(defun count-rec (chance healthy infected limit period time list-of-points)
  (if (<= (- limit infected) 0.001)
      list-of-points
      (let* ((new-infected (floor (* (/ healthy 100) chance) 1.0))
             (still-healthy (- healthy new-infected)))
        (setf list-of-points (append list-of-points (list
                                                     (cons (+ time period)
                                                           (+ new-infected infected)))))
        (count-rec chance still-healthy (+ new-infected infected)
                   limit period (+ period time) list-of-points))))


(defun cnt (chance amount limit period)
  (count-rec chance amount period limit period 0 (list (cons 0 0))))

(defun generate-corona-graphic-desease (array chance healthy infected-limit period)
  (let ((list-of-points (cnt chance healthy infected-limit period)))
    (generate-graphic array list-of-points 34 65 23)
    (save-png (array-dimension array 0)
              (array-dimension array 1)
              "./test.png"
              array)))
