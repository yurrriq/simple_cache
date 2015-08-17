;;;-------------------------------------------------------------------
;;; @author  YOUR NAME, <YOUR EMAIL>
;;; @copyright (C) YEAR,
;;; @doc
;;;     YOUR PROJECT application server
;;; @end
;;; Created : TODAY by YOUR EMAIL
;;;-------------------------------------------------------------------
(defmodule sc-element
  (behaviour gen_server)
  ;; API
  (export (start_link 2)
          (create 2)
          (create 1)
          (fetch 1)
          (replace 2)
          (delete 1))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defun server-name () 'sc-element)

(defun default-lease-time () (* 60 60 24))

(defrecord state value lease-time start-time)


;;;===================================================================
;;; API
;;;===================================================================

;;--------------------------------------------------------------------
;; @doc
;; Starts the server
;;
;; @spec start_link() -> {ok, Pid} | ignore | {error, Error}
;; @end
;;--------------------------------------------------------------------
(defun start_link (value lease-time)
  (gen_server:start_link (MODULE) `(,value ,lease-time) '()))

(defun create (value lease-time) (sc-sup:start_child value lease-time))

(defun create (value) (create value (default-lease-time)))

(defun fetch (pid) (gen_server:call pid 'fetch))

(defun replace (pid value) (gen_server:cast pid `#(replace ,value)))

(defun delete (pid) (gen_server:cast pid 'delete))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Initializes the server
;;
;; @spec init(Args) -> {ok, State} |
;;                     {ok, State, Timeout} |
;;                     ignore |
;;                     {stop, Reason}
;; @end
;;--------------------------------------------------------------------
(defun init
  ([`(,value ,lease-time)]
   (let* ((now        (calendar:local_time))
          (start-time (calendar:datetime_to_gregorian_seconds now)))
     `#(ok
        ,(make-state value      value
                     lease-time lease-time
                     start-time start-time)
        ,(time-left start-time lease-time)))))


;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling call messages
;;
;; @spec handle_call(Request, From, State) ->
;;                                   {reply, Reply, State} |
;;                                   {reply, Reply, State, Timeout} |
;;                                   {noreply, State} |
;;                                   {noreply, State, Timeout} |
;;                                   {stop, Reason, Reply, State} |
;;                                   {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_call
  (['fetch _from state]
   (let* (((match-state value      value
                        lease-time lease-time
                        start-time start-time) state)
          (time-left (time-left start-time lease-time)))
     `#(reply #(ok ,value) ,state ,time-left))))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling cast messages
;;
;; @spec handle_cast(Msg, State) -> {noreply, State} |
;;                                  {noreply, State, Timeout} |
;;                                  {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_cast
  ([`#(replace ,value) state]
   (let* (((match-state lease-time lease-time
                        start-time start-time) state)
          (time-left (time-left start-time lease-time)))
     `#(noreply ,(set-state-value state value) ,time-left)))
  (['delete state] `#(stop normal ,state)))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Handling all non call/cast messages
;;
;; @spec handle_info(Info, State) -> {noreply, State} |
;;                                   {noreply, State, Timeout} |
;;                                   {stop, Reason, State}
;; @end
;;--------------------------------------------------------------------
(defun handle_info (['timeout state] `#(stop normal ,state)))

;;--------------------------------------------------------------------
;; @private
;; @doc
;; This function is called by a gen_server when it is about to
;; terminate. It should be the opposite of Module:init/1 and do any
;; necessary cleaning up. When it returns, the gen_server terminates
;; with Reason. The return value is ignored.
;;
;; @spec terminate(Reason, State) -> void()
;; @end
;;--------------------------------------------------------------------
(defun terminate (_reason _state)
  (sc-store:delete (self))
  'ok)

;;--------------------------------------------------------------------
;; @private
;; @doc
;; Convert process state when code is changed
;;
;; @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
;; @end
;;--------------------------------------------------------------------
(defun code_change (_old-version state _extra) `#(ok ,state))

;;;===================================================================
;;; Internal functions
;;;===================================================================

(defun time-left
  ([_start-time 'infinity] 'infinity)
  ([start-time lease-time]
   (let* ((now          (calendar:local_time))
          (current-time (calendar:datetime_to_gregorian_seconds now))
          (time-elapsed (- current-time start-time)))
     (case (- lease-time time-elapsed)
       (time (when (=< time 0)) 0)
       (time                    (* time 1000))))))
