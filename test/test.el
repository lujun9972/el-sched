(require 'ert)

(ert-deftest test-list< ()
  (should (sched-list< nil '(1)))
  (should (sched-list< '(1) '(1 2)))
  (should (sched-list< '(1 1) '(1 2)))
  (should (sched-list< '(1 2) '(2 1)))
  (should (sched-list< '((1 2) 1 2) '((2 1) 1 2)))
  (should (sched-list< '((1) 1 2) '((2) 1 )))
  (should (sched-list< '("abc") '("bcd")))
  (should (sched-list< '("abc") '("abc" 2)))
  (should (sched-list< '(("abc") 1 2) '(("bcd") 1 2)))
  (should (not (sched-list< '("abc") '("abc"))))
  (should (not (sched-list< '("abd") '("abc"))))
  (should (not (sched-list< '(2) '(1))))
  (should (not (sched-list< '(1 2) '(1)))))

(ert-deftest test-sched-enter ()
  ""
  (let ((sched1 (make-scheduler)))
    (should (string-equal "task0task1task2task3"
                          (with-output-to-string
                            (sched-enter sched1 1 50 #'princ "task0")
                            (sched-enter sched1 1 100 #'princ "task1")
                            (sched-enter sched1 2 50 #'princ "task2")
                            (sched-enter sched1 2 100 #'princ "task3")
                            (sched-run sched1))))))

(ert-deftest test-sched-cancel ()
  ""
  (let ((sched1 (make-scheduler))
        event-canceled )
    (should (string-equal "task0task1task2task3"
                          (with-output-to-string
                            (sched-enter sched1 1 50 #'princ "task0")
                            (sched-enter sched1 1 100 #'princ "task1")
                            (sched-enter sched1 2 50 #'princ "task2")
                            (setq event-canceled (sched-enter sched1 1 10 #'princ "event-canceled"))
                            (sched-enter sched1 2 100 #'princ "task3")
                            (sched-cancel sched1 event-canceled)
                            (sched-run sched1))))))

(ert-deftest test-sched-empty-p ()
  ""
  (let ((sched1 (make-scheduler)))
    (should (sched-empty-p sched1))
    (sched-enter sched1 1 50 #'princ "task0")
    (should (not (sched-empty-p sched1)))))
