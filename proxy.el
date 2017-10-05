(require 'deferred)

(setq counter (make-hash-table :test 'equal))
(setq blacklist '(""))

(defun put-access-time (ip)
  (let ((access-times (gethash ip counter)))
    (puthash ip
             (push (truncate (float-time)) access-times)
             counter)))

(defun edos? (ip)
  (let* ((access-times (put-access-time ip))
         (length (length access-times))
         (latest (first access-times))
         (oldest (or (nth 9 access-times) (last access-times)))
         (attacked? (and (> length 10) (> 5 (- latest oldest))))
         (blacklisted? (member ip blacklist)))
    (when (and attacked? (not blacklisted?)) (push ip blacklist))
    (or attacked? blacklisted?)))

(defun render-service-unavailable (httpcon)
  (elnode-http-start httpcon 503 '("Content-Type" . "text/html"))
  (elnode-http-return httpcon "<html><h1>Service unavailable.</h1></html>"))

(defun forward-to-backend (httpcon method path)
  (deferred:sync!
    (deferred:$
      (deferred:url-retrieve (format "http://localhost:3000%s" path))
      (deferred:nextc it
        (lambda (buf)
          (let ((res (with-current-buffer buf (buffer-string))))
            (kill-buffer buf)
            (if (string-suffix-p ".css" path)
                (elnode-http-start httpcon 200 '("Content-Type" . "text/css"))
              (elnode-http-start httpcon 200 '("Content-Type" . "text/html")))
            (elnode-http-return httpcon res)))))))

(defun proxy-handler (httpcon)
  (let ((method (elnode-http-method httpcon))
        (path (elnode-http-pathinfo httpcon))
        (remote-addr (replace-regexp-in-string ":[0-9]+" "" (elnode-remote-ipaddr httpcon))))
    (if (edos? remote-addr) (render-service-unavailable httpcon) (forward-to-backend httpcon method path))))

(elnode-start 'proxy-handler :port 8000 :host "localhost")
