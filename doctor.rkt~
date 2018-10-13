; заготовка "Доктора". Март 2018
#lang scheme/base
; В учебных целях используется базовая версия Scheme

(define keys '( 
  (
    (depressed suicide exams university)
    (
     (when you feel depressed, go out for ice cream)
     (depression is a disease that can be treated)
     (it will get better ..., just kidding)
     (you chose yourself to study there)
     (stop complaining and go БOTATb)
    )
  )
  (
    (mother father parents brother sister uncle ant grandma grandpa)
    (
     (tell me more about your * , i want to know all about your *)
     (why do you feel that way about your * ?)
     (whats wrong with your *)
     (its typical for *)
     (you are not alone when it comes to dealing with *)
    )
  )
  (
    (university scheme lections)
    (
     (your education is important)
     (how many time do you spend to learning ?)
     (you should have payed more attention to english lessons)
     (personally, i think that scheme is awesome)
     (msu for the win!!1!!!!)
    )
  )
))

(define all-keywords
  (foldl (lambda (x y) (append (filter (lambda (w) (not (member w y))) (car x)) y)) '() keys))



; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor stop-word patients-num)
  (cond ((< patients-num 1) (printf "goodbye/n")) 
    (else (print 'next!)
     (newline)
    (printf "who are you?")
    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond 
        ((eq? (car user-response) stop-word)
          (print '(ok, goodbye)))
        (else
          (printf "what seems to be the problem, ~a?" (car user-response))
          (doctor-driver-loop (car user-response) '())
          (visit-doctor stop-word (- patients-num 1))))))))
  ; (printf "Hello, ~a!\n" name)
;   (print '(what seems to be the trouble?))
;   (doctor-driver-loop name '()) 
; )

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала 
(define (qualifier-answer-pred x . rest) #t)
(define (qualifier-answer user-response prev-responses)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (you think that)
                               (you say that)
                               (why do you think that)
                               (why do you feel that))
                )
                (change-person user-response)
        )
 )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-pred x . rest) #t)
(define (hedge . rest)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (sometimes i think that myself)
                       (tell me more)
                       (i am listening)
                       )
         )
)
; 3й способ -- на основе предыдущих ответов
(define (history-answer-pred user-response prev-responses)
  (> (length prev-responses) 3))
(define (history-answer user-response prev-responses)
  (append '(earlier you said that) (change-person (pick-random prev-responses))))

; 4й способ -- на основе ключевых слов в ответе
(define (keywords-strategy-pred response prev-responses)
  (ormap (lambda (x) (if (member x all-keywords) #t #f)) response))
(define (keywords-strategy response prev-responses)
  (let* ((response-keywords (filter (lambda (x) (member x all-keywords)) response))
         (chosen-keyword (pick-random response-keywords))
         (valid-replies
                 (foldl (lambda (key-group rest) (
                               cond ((member chosen-keyword (car key-group))
                                         (append (cadr key-group) rest))
                                     (else rest))) '() keys))
         (chosen-reply (pick-random valid-replies))
         (reply (many-replace (list (list '* chosen-keyword)) chosen-reply)))
    reply))


; стратегия ответа
(define strategy 
  (list
   (list 1 hedge-pred hedge)
   (list 1000 qualifier-answer-pred qualifier-answer)
   (list 1 history-answer-pred history-answer)
   (list 1 keywords-strategy-pred keywords-strategy)
  )
)



; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name prev-responses)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else (print (reply strategy user-response prev-responses)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response prev-responses))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply strategy user-response prev-responses)
  (define (choose-with-weights random-left lst)
    (let ((after-random (- random-left (caar lst))))
      (if (< after-random 0)
          (caddar lst)
          (choose-with-weights after-random (cdr lst)))))
  (let* ((valid-strats (filter (lambda (strat) ((list-ref strat 1) user-response prev-responses)) strategy))
         (weight-sum (foldl (lambda (strat sum) (+ sum (car strat))) 0 valid-strats)))
  ((choose-with-weights (random weight-sum) valid-strats) user-response prev-responses)))


;(define (reply strategy user-response prev-responses)
;  (let ((weighted-strats (foldl (lambda (strat rest) (append (build-list (car strat) (lambda (x) (caddr strat))) rest)) '() (filter (lambda (strat) ((list-ref strat 1) user-response prev-responses)) strategy))))
;   ((pick-random weighted-strats) user-response prev-responses)))

;(define (reply user-response prev-responses) 
;  (if (keys-pred user-response)
;      (keywords-strategy user-response)
;      (
;  (let ((rand (if (>= (length prev-responses) 3) (random 3) (random 2))))
;    (cond ((= rand 0) (hedge))
;          ((= rand 1) (qualifier-answer user-response))
;          (else (history-answer prev-responses)))))))
  





; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        (many-replace '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
			(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
			(yourself myself))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (map (lambda (x) (let ((rep (assoc x replacement-pairs)))
                   (if rep (cadr rep) x))) lst))


;(define (many-replace replacement-pairs lst)
;(define (helper lst res replacement-pairs)
;  (if (null? lst) (reverse res)
;      (begin (let ((pat-rep (assoc(car lst) replacement-pairs)))
;               (helper (cdr lst) (cons (if pat-rep (cadr pat-rep) (car lst)) res) replacement-pairs)))))
;(helper lst '() replacement-pairs))
  
;(define (many-replace replacement-pairs lst)
;        (cond ((null? lst) lst)
;              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
;                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
;                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
;                            )
;                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
;                        )))))



(visit-doctor 'superstop 3)