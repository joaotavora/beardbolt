(require 'beardbolt)
(require 'cl-lib)
(require 'benchmark)

(defvar beardbolt-benchmark-samples nil)
(defvar beardbolt-benchmark-rmsbolt-samples nil)

(advice-add (quote beardbolt--handle-finish-compile) :around
            (lambda (oldfun &rest args)
              (push (benchmark-elapse (apply oldfun args))
                    beardbolt-benchmark-samples)))

(defun beardbolt-benchmark-beardbolt (repeats)
  (cl-loop
   repeat repeats
   do (call-interactively #'beardbolt-compile)
   (while (process-live-p
           (get-buffer-process (beardbolt--compilation-buffer)))
     (accept-process-output)))
  (message "Beardbolt timings for %s\n  samples: %s\n  average: %.3fs"
           (file-name-nondirectory buffer-file-name)
           (mapcar (lambda (s) (format "%.3fs" s))
                   (reverse beardbolt-benchmark-samples))
           (/ (cl-reduce #'+ beardbolt-benchmark-samples)
              (length beardbolt-benchmark-samples) 1.0)))

(defun beardbolt-benchmark-rmsbolt (repeats)
  (add-to-list 'load-path (expand-file-name "../../rmsbolt" default-directory))
  (cond ((require 'rmsbolt nil t)
         (advice-add (quote rmsbolt--handle-finish-compile) :around
                     (lambda (oldfun &rest args)
                       (push (benchmark-elapse (apply oldfun args))
                             beardbolt-benchmark-rmsbolt-samples)))
         (cl-loop
          repeat repeats
          do
          (sit-for 0.2) ;; unconfuse RMSbolt state cleanup
          (rmsbolt-compile)
          (while (process-live-p
                  (get-buffer-process (get-buffer "*rmsbolt-compilation*")))
            (accept-process-output)))
         (message "RMSbolt timings for %s\n  samples: %s\n  average: %.3fs"
                  (file-name-nondirectory buffer-file-name)
                  (mapcar (lambda (s) (format "%.3fs" s)) (reverse
                                                           beardbolt-benchmark-rmsbolt-samples))
                  (/ (cl-reduce #'+ beardbolt-benchmark-rmsbolt-samples)
                     (length beardbolt-benchmark-rmsbolt-samples) 1.0)))
        (t
         (message "Can't find rmsbolt in load path %s" load-path))))

(with-current-buffer (find-file (car argv))
  (beardbolt-benchmark-rmsbolt 5)
  (beardbolt-benchmark-beardbolt 5))




