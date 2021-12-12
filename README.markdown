# CAS-MIDDLEWARE

Add CAS authentication to [Caveman](https://github.com/fukamachi/caveman) Web Application.

Use CL-CAS system (https://github.com/fferrere/cl-cas)

Provide :
- Authentication by default for the entire application
- Access to CAS user name and attribtues
- CAS Logout

## Installation
1. git clone https://github.com/fferrere/cl-cas
2. git clone https://github.com/fferrere/cas-middleware
3. Add cas-middleware system to your Caveman project (asd file) `:depends-on ("cas-middleware" ...)`
4. Add config data to <Your Caveman Project>/src/config.lisp
5. Add middleware to <Your Caveman Project>/app.lisp

## System Nicknames
- lack.middleware.auth.cas : mandatory for caveman
- cm : to use cas-logout and cas-authenticated-user functions

## Usage

By default all routes requires authenticated session, but you can exclude some routes, like a Welcome page.

### Configuration (Add to Caveman config.lisp file) :

- [Required] CAS Server URL
- [Optional] Authentication mode : single sign-on (SSO) or force (renew)
- [Optional] Excludes routes

Example (SSO : On - No excludes routes) :
```
(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
      :cas (:client (:server-url "https://cas-test.u-bordeaux.fr/cas"))))
```

Example (SSO : On - Excludes routes) :
```
(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
      :cas (:client (:server-url "https://cas-test.u-bordeaux.fr/cas") :excludes ("/"))))
```

Example (SSO Off, force authentication - No excludes routes) :
```
(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
      :cas (:client (:server-url "https://cas-test.u-bordeaux.fr/cas" :renew t))))
```

### Add CAS-Middleware component to your app (app.lisp file)

Exemple :

```
(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
(:auth-cas :config (getf (getf (config) :cas) :client)
           :excludes (getf (getf (config) :cas) :excludes))
 *web*)
```

### User name and attributes

CAS-Middleware stores user name and attributes into Caveman session. To get this data, use :
```
(multiple-value-bind (username attributes) (cas-authenticated-user *session*)
  <body>
  )
```

### CAS Logout

`(cas-logout *session*)`

If redirect-url is supported bye the server

`(cas-logout *session* redirect-url)`



## Demo

See https://github.com/fferrere/cas-demo

## Author

* Frédéric FERRERE (frederic.ferrere@gmail.com)

## Licence

Apache-2.0 (https://www.apache.org/licenses/LICENSE-2.0)


