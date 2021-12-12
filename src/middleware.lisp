(in-package :cas-middleware)

(defvar *cas-client*)

(defun request-url (env)
  (format nil "~a://~a:~a~a"
          (getf env :url-scheme)
          (getf env :server-name)
          (getf env :server-port)
          (getf env :path-info)))

(defun cookie-session-id (env)
  (cdr (assoc "lack.session" (getf env :cookies) :test #'string=)))

(defun stored-session-id (env)
  (gethash :session-id (getf env :lack.session) nil))

(defun cas-logout (config session &optional logout-url)
  (let ((cas (apply #'make-instance 'cas-client config)))
    (remhash :session-id session)
    (remhash :user-uid session)
    (remhash :user-attrs session)
    (return-302 (cas-logout-url cas logout-url))))

(defun cas-authenticated-user (session)
  (values (gethash :user-uid session nil) (gethash :user-attrs session nil)))

(defun authenticatedp (env)
  "Check if Cookie Session ID is set and equal to stored session ID"
  (when-let ((stored-session-id (stored-session-id env)))
    (let ((cookie-session-id (cookie-session-id env)))
      (string= cookie-session-id stored-session-id))))

(defun session-store-cas-user (env uid &optional (attributes ()))
  (setf (gethash :user-uid (getf env :lack.session) nil) uid)
  (when attributes
    (setf (gethash :user-attrs (getf env :lack.session) nil) attributes)))

(defun session-store-cas-session-id (env)
  (setf (gethash :session-id (getf env :lack.session) nil) (cookie-session-id env)))

(defun auth-cas-ticket (env)
  "If exists, return cas-ticket from URL"
  (let ((qs (getf env :query-string)))
    (and qs (cas:cas-ticket qs))))

(defun allow-user-request (env uid &optional (attributes ()))
  "Stores user data returned by CAS server in the sessions.
   Then redirects the user to the requested page.
   The redirection is necessary to avoid displaying the CAS ticket in the URL (See CAS Protocol)."
  (session-store-cas-user env uid attributes)
  (session-store-cas-session-id env)
  (return-302 (request-url env)))

(defparameter *lack-middleware-auth-cas*
  (lambda (app &key config excludes)
    (lambda (env)
      (let ((*cas-client* (apply #'make-instance 'cas-client config))
            (route (getf env :path-info)))
       (if (find route excludes :test #'string=)
	   (funcall app env)
	   (if (authenticatedp env)
	       (funcall app env)
               (let ((service-url (request-url env)))
                 (if-let ((ticket (auth-cas-ticket env)))
                   (multiple-value-bind (username attributes)
                       (cas:cas-service-validate *cas-client* service-url ticket)
                     (if username
                       (allow-user-request env username attributes)
                       (return-500)))
                   (return-302 (cas:cas-login-url *cas-client* service-url)))))))))
    "Middleware for CAS Authentication")

(defun return-500 ()
  `(500
    (:content-type "text/plain"
     :content-length 52)
    ("Internal Server Error : Cas ticket validation failed")))

(defun return-302 (url)
  `(302 (:Location ,url)))

(defun return-401 ()
  `(401 (:content-type "text/plain"
         :content-length 12)
        ("Unauthorized")))


