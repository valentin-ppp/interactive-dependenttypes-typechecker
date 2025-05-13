;; Some integer related examples

;; None
(<=-compile *0*)

;; *0* <= top
(<=-compile (*0* *0*))

;; (<=p 'm FUN XXX)
;; (<=p XXX 'm)
(<=-compile ('m 'm))
