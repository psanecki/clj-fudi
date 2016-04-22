(ns clj-fudi.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net
            Socket
            ServerSocket
            DatagramSocket
            DatagramPacket
            InetSocketAddress]
           [java.io
            DataOutputStream
            DataInputStream
            StringWriter]))

(defn- a->str
  [a]
  (cond
    (number? a) (str (float a))
    (keyword? a) (name a)
    (instance? java.lang.Boolean a) (if a "1" "0")
    (string? a) a
    :else ""))

(defn dict->fudi-str-seq
  [obj-msg]
  (for [k (keys obj-msg)]
    (let [message (k obj-msg)
          message (if (coll? message)
                    (let [message-str
                          (map a->str message)]
                      (->>
                       message-str
                       (interpose " ")
                       (reduce str "list ")))
                   (a->str message))]
      (str/join  [(name k) " " message ";\r\n"]))))




(defn send-udp
  [obj-msg & [host port]]
  ;;   ex. (send-udp {:a 2 :b (vec (range 0 10))} :127.0.0.1 3001)

  (let [host (or host :localhost)
        port (or port 3000)
        socket (new DatagramSocket)
        messages (dict->fudi-str-seq obj-msg)
        buffers (map (fn [m] (.getBytes m)) messages)
        address (new InetSocketAddress (name host) port)
        packets (map  (fn [b]
                        (new DatagramPacket b  (alength b) address))
                      buffers)]
    (doseq [packet packets] (.send socket packet))
    (.close socket)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- receive
  "Read a line of textual data from the given socket"
  [socket]
  (.readLine (io/reader socket)))

(defn- send1
  "Send the given string message out over the given socket"
  [socket msg]
  (let [writer (io/writer socket)]
    (prn (type msg))
    (prn msg)
    (.write writer msg)
    (.flush writer)))

(defn serve-tcp
  "Handler must return map of dictionaries"
  [handler port]
  (with-open [server-sock (ServerSocket. port)
              sock (.accept server-sock)]
    (let [msg-in (receive sock)
          msg-out (-> msg-in handler dict->fudi-str-seq)
          writer (io/writer sock)]
      (doseq [m msg-out] (.write writer m))
      (.flush writer))))

(defn serve-persistent-tcp [handler port]
  (let [running (atom true)]
    (future
      (with-open [server-sock (ServerSocket. port)]
        (while @running
          (with-open [sock (.accept server-sock)]
            (let [msg-in (receive sock)
                  msg-out (-> msg-in handler dict->fudi-str-seq)
                  writer (io/writer sock)]
              (doseq [m msg-out] (.write writer m))
              (.flush writer))))))
    running))




;;;;;;;;;;;;;;;
(defn send-tcp
  [obj-msg host port]
  ;;   ex. (send-tcp {:a 2 :b [2]}  :localhost 3000)
  (try
    (let [socket (new Socket (name host) port)
          out (.getOutputStream socket)
          dos (new DataOutputStream out)
          messages (dict->fudi-str-seq obj-msg)
          buffer (.getBytes (str/join messages))
          ]
      (doto dos
        (.write buffer 0 (alength buffer))
         .flush
         .close
        ))
    (catch Exception e (.printStackTrace e))))


(defn send-fudi
  [message port]
  (with-open [sock (new Socket "localhost" port)
              writer (io/writer sock)]
    (doto writer
      (.append (str message "\r\n"))
      (.flush))))
