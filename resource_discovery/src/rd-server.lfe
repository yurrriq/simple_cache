(defmodule rd-server
  (behaviour gen_server)
  ;; API
  (export (start_link 0)
          (add-local-resource 2)
          (add-target-resource-type 1)
          (fetch-resources 1)
          (trade-resources 0))
  ;; gen_server callbacks
  (export (init 1)
          (handle_call 3)
          (handle_cast 2)
          (handle_info 2)
          (terminate 2)
          (code_change 3)))

(defun SERVER () (MODULE))

(defrecord state
  (target-resource-types '())
  (local-resource-tuples (dict:new))
  (found-resource-tuples (dict:new)))


;;;===================================================================
;;; API
;;;===================================================================

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER)) (MODULE) '() '()))

(defun add-local-resource (type resource)
  (gen_server:cast (SERVER) `#(add-local-resource #(,type ,resource))))

(defun add-target-resource-type (type)
  (gen_server:cast (SERVER) `#(add-target-resource-type ,type)))

(defun fetch-resources (type)
  (gen_server:call (SERVER) `#(fetch-resources ,type)))

(defun trade-resources () (gen_server:cast (SERVER) 'trade-resources))


;;;===================================================================
;;; gen_server callbacks
;;;===================================================================

(defun init (['()] `#(ok ,(make-state))))

(defun handle_call
  ([`#(fetch-resources ,type) _from state]
   `#(reply ,(dict:find type (state-found-resource-tuples state)) ,state)))

(defun handle_cast
  ([`#(add-local-resource #(,type ,resource)) state]
   (let* ((resources  (state-local-resource-tuples state))
          (resources* (add-resource type resource resources)))
     `#(noreply ,(set-state-local-resource-tuples state resources*))))
  ([`#(add-target-resource-type ,type) state]
   (let* ((types (state-target-resource-types state))
          (types* `(,type . ,(lists:delete type types))))
     `#(noreply ,(set-state-target-resource-types state types*) state)))
  ([`#(trade-resources #(,reply-to ,remotes))
    (= state (match-state target-resource-types types
                          local-resource-tuples locals
                          found-resource-tuples found))]
   (let ((found* (add-resources (resources-for-types types remotes) found)))
     (case reply-to
       ('noreply 'ok)
       (_        (gen_server:cast `#(,(SERVER) ,reply-to)
                                  `#(trade-resources #(noreply ,locals)))))
     `#(noreply ,(set-state-found-resource-tuples state found*)))))

(defun handle_info ([(= 'ok _info) state] `#(noreply ,state)))

(defun terminate (_reason _state) 'ok)

(defun code_change (_old-version state _extra) `#(ok ,state))


;;;===================================================================
;;; Utilities
;;;===================================================================

(defun add-resources
  ([`(#(,type ,id) . ,t) dict] (add-resources t (add-resource type id dict)))
  (['() dict]                  dict))

(defun add-resource (type resource dict)
  (case (dict:find type dict)
    (`#(ok ,resources)
     (dict:store type `(,resource . ,(lists:delete resource resources)) dict))
    ('error (dict:store type `(,resource) dict))))

(defun resources-for-types (types resources)
  (let ((f (lambda (type acc)
             (case (dict:find type resources)
               (`#(ok ,lst) (++ (lists:map (lambda (r) `#(,type ,r)) lst) acc))
               ('error      acc)))))
    (lists:foldl f '() types)))
