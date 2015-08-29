- [`sc-element`](#sec-1)
  - [API](#sec-1-1)
    - [`start_link/2`](#sec-1-1-1)
    - [`create/2` and `create/1`](#sec-1-1-2)
    - [`fetch/1`, `replace/2` and `delete/1`](#sec-1-1-3)
  - [`gen_server` callbacks](#sec-1-2)
    - [`init/1`](#sec-1-2-1)
    - [`handle_call/3`](#sec-1-2-2)
    - [`handle_cast/2`](#sec-1-2-3)
    - [`handle_info/2`](#sec-1-2-4)
    - [`terminate/2`](#sec-1-2-5)
    - [`code_change/3`](#sec-1-2-6)
  - [Internal functions](#sec-1-3)
    - [`time-left/2`](#sec-1-3-1)


# `sc-element`<a id="sec-1" name="sec-1"></a>


```lfe
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
```

```lfe
(defun server-name () 'sc-element)
```

We define the `default-lease-time` as one day in seconds, i.e. `60` (seconds in
a minute) times `60` (minutes in an hour) times `24` (hours in a day).

```lfe
(defun default-lease-time () (* 60 60 24))
```

To manage the state of stored values and their lease times, we define the
`state` record, which contains the self-descriptive keys, `value`, `lease-time`
and `start-time`.

```lfe
(defrecord state value lease-time start-time)
```

## API<a id="sec-1-1" name="sec-1-1"></a>

### `start_link/2`<a id="sec-1-1-1" name="sec-1-1-1"></a>

We define our own `start_link/2` to hide mask the `gen_server:start_link/3`
call.

```lfe
(defun start_link (value lease-time)
  (gen_server:start_link (MODULE) `(,value ,lease-time) '()))
```

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Argument</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">`value`</td>
<td class="left">Any LFE term to be stored.</td>
</tr>


<tr>
<td class="left">`lease-time`</td>
<td class="left">The number of seconds this element should be available.</td>
</tr>
</tbody>
</table>

Later on, this will get called by `sc-element-sup:start_child/1`, which is
called by `sc-element:create/2`.

### `create/2` and `create/1`<a id="sec-1-1-2" name="sec-1-1-2"></a>

In order to create a new element, we at least need a value. Along with a value,
`create/2` takes a lease time. With that, we call
`sc-element-sup:start_child/2`, which we'll implement later.

```lfe
(defun create  (value lease-time) (sc-element-sup:start_child value lease-time))
```

Under the hood, `create/1` simply calls `create/2` with `(default-lease-time)`
as the lease time.

```lfe
(defun create  (value)            (create value (default-lease-time)))
```

### `fetch/1`, `replace/2` and `delete/1`<a id="sec-1-1-3" name="sec-1-1-3"></a>

These three API functions are simply convenient wrappers around the appropriate
`gen_server:call/2` and `gen_server:cast/2` calls.

```lfe
(defun fetch   (pid)              (gen_server:call pid 'fetch))

(defun replace (pid value)        (gen_server:cast pid `#(replace ,value)))

(defun delete  (pid)              (gen_server:cast pid 'delete))
```

## `gen_server` callbacks<a id="sec-1-2" name="sec-1-2"></a>

### TODO `init/1`<a id="sec-1-2-1" name="sec-1-2-1"></a>

```lfe
(defun init
  ([`(,value ,lease-time)]
   (let* ((now        (calendar:local_time))
          (start-time (calendar:datetime_to_gregorian_seconds now)))
     `#(ok
        ,(make-state value      value
                     lease-time lease-time
                     start-time start-time)
        ,(time-left start-time lease-time)))))
```

### TODO `handle_call/3`<a id="sec-1-2-2" name="sec-1-2-2"></a>

```lfe
(defun handle_call
  (['fetch _from state]
   (let* (((match-state value      value
                        lease-time lease-time
                        start-time start-time) state)
          (time-left (time-left start-time lease-time)))
     `#(reply #(ok ,value) ,state ,time-left))))
```

### TODO `handle_cast/2`<a id="sec-1-2-3" name="sec-1-2-3"></a>

```lfe
(defun handle_cast
  ([`#(replace ,value) state]
   (let* (((match-state lease-time lease-time
                        start-time start-time) state)
          (time-left (time-left start-time lease-time)))
     `#(noreply ,(set-state-value state value) ,time-left)))
  (['delete state] `#(stop normal ,state)))
```

### TODO `handle_info/2`<a id="sec-1-2-4" name="sec-1-2-4"></a>

```lfe
(defun handle_info (['timeout state] `#(stop normal ,state)))
```

### TODO `terminate/2`<a id="sec-1-2-5" name="sec-1-2-5"></a>

```lfe
(defun terminate (_reason _state)
  (sc-store:delete (self))
  'ok)
```

### TODO `code_change/3`<a id="sec-1-2-6" name="sec-1-2-6"></a>

```lfe
(defun code_change (_old-version state _extra) `#(ok ,state))
```

## TODO Internal functions<a id="sec-1-3" name="sec-1-3"></a>

### TODO `time-left/2`<a id="sec-1-3-1" name="sec-1-3-1"></a>

```lfe
(defun time-left
  ([_start-time 'infinity] 'infinity)
  ([start-time lease-time]
   (let* ((now          (calendar:local_time))
          (current-time (calendar:datetime_to_gregorian_seconds now))
          (time-elapsed (- current-time start-time)))
     (case (- lease-time time-elapsed)
       (time (when (=< time 0)) 0)
       (time                    (* time 1000))))))
```
