;; """A generally useful event scheduler class.

;; Each instance of this class manages its own queue.
;; No multi-threading is implied; you are supposed to hack that
;; yourself, or use a single instance per application.

;; Each instance is parametrized with two functions, one that is
;; supposed to return the current time, one that is supposed to
;; implement a delay.  You can implement real-time scheduling by
;; substituting time and sleep from built-in module time, or you can
;; implement simulated time by writing your own functions.  This can
;; also be used to integrate scheduling with STDWIN events; the delay
;; function is allowed to modify the queue.  Time can be expressed as
;; integers or floating point numbers, as long as it is consistent.

;; Events are specified by tuples (time, priority, action, argument).
;; As in UNIX, lower priority numbers mean higher priority; in this
;; way the queue can be maintained as a priority queue.  Execution of the
;; event means calling the action function, passing it the argument
;; sequence in "argument" (remember that in Python, multiple function
;; arguments are be packed in a sequence).
;; The action function may be an instance method so it
;; has another way to reference private data (besides global variables).
;; """

(require 'cl-lib)

(cl-defstruct scheduler
  (queue nil)
  (timefunc #'float-time)
  (delayfunc #'sit-for))

(defun sched-list< (list1 list2)
  (cl-check-type list1 list)
  (cl-check-type list2 list)
  (cond ((null list1)
         list2)
        ((null list2)
         nil)
        (t (let ((ele1 (car list1))
                 (ele2 (car list2)))
             (assert (equal (type-of ele1)
                            (type-of ele2)))
             (or (cond ((numberp ele1)
                        (< ele1 ele2))
                       ((stringp ele1)
                        (string-lessp ele1 ele2))
                       ((listp ele1)
                        (list< ele1 ele2))
                       (t (error "unsupport type:%s" (type-of ele1))))
                 (and (equal ele1 ele2)
                      (list< (cdr list1) (cdr list2))))))))

(defun sched-enterabs (scheduler time priority action &rest arguments)
  "Enter a new event in the queue at an absolute time.

Returns the event which can be used to remove it,if necessary. "
  (let ((event (list time priority action arguments)))
    (setf (scheduler-queue scheduler)
          (sort (cons event (scheduler-queue scheduler)) #'sched-list<))
    event))

(defun sched-enter (scheduler delay priority action &rest arguments)
  "A variant that specifies the time as a relative time.

This is actually the more commonly used interface. "
  (let ((time  (+ delay (funcall (scheduler-timefunc scheduler)))))
    (apply #'sched-enterabs scheduler time priority action arguments)))

(defun sched-cancel (scheduler event)
  "Remove an event from the queue.

This must be presented the event as returned by enter().
If the event is not in the queue, do nothing. "
  (setf (scheduler-queue scheduler)
        (remove event (scheduler-queue scheduler))))

(defun sched-empty-p (scheduler)
  "Check whether the queue is empty"
  (null (scheduler-queue scheduler)))

(defun sched-run (scheduler)
  "Execute events until the queue is empty.

When there is a positive delay until the first event, the
delay function is called and the event is left in the queue;
otherwise, the event is removed from the queue and executed
(its action function is called, passing it the argument).  If
the delay function returns prematurely, it is simply
restarted.

It is legal for both the delay function and the action
function to to modify the queue or to raise an exception;
exceptions are not caught but the scheduler's state remains
well-defined so run() may be called again.

A questionable hack is added to allow other threads to run:
just after an event is executed, a delay of 0 is executed, to
avoid monopolizing the CPU when other threads are also
runnable. "
(let ((q (scheduler-queue scheduler))
      (delayfunc (scheduler-delayfunc scheduler))
      (timefunc (scheduler-timefunc scheduler)))
  (while q
    (cl-multiple-value-bind (time priority action arguments) (pop q)
      (let ((now (funcall timefunc)))
        (when (< now time)
          (funcall delayfunc (- time now)))
        (apply action arguments))))))
