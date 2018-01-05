(defun reschedule-timer (timer delay)
  (when timer
    (mp:schedule-timer-relative-milliseconds timer (/ delay 1000))))
