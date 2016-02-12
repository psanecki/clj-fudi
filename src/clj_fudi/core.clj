(ns clj-fudi.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.net
            Socket
            DatagramSocket
            DatagramPacket
            InetSocketAddress]
           [java.io
            DataOutputStream
            DataInputStream
            StringWriter]))



(defn dict->fudi-str-seq [obj-msg]
  (for [k (keys obj-msg)]
    (let [message (k obj-msg)
          message (if (coll? message)
                    (let [message-str
                          (map
                           #(cond
                              (number? %) (str (float %))
                              (keyword? %) (name %)
                              (instance? java.lang.Boolean %) (if % "1" "0")
                              (string? %) %
                              :else "")
                           message)]
                      (->>
                       message-str
                       (interpose " ")

                       (reduce str "list ")))
                    ;; (if (number? (first message))
                    ;;   (->>
                    ;;    (interpose " " (map (comp str float) message))
                    ;;    (reduce str "list "))
                    ;;   (->>
                    ;;    (interpose " "  message)
                    ;;    (reduce str "list ")))
                    ;; else
                    (if (number? message)
                      (str (float message))
                      message))]
      (reduce str (name k) [" " message ";\n"]))
    ;; (apply str (name k) " "
    ;;      (let [message (k obj-msg)]
    ;;        (if (coll? message)
    ;;          (let [messages (k obj-msg)
    ;;                messages (if (number? (first messages))
    ;;                           (map float messages)
    ;;                           (map str messages))]
    ;;            (apply str "list"
    ;;                   " "
    ;;                   (reduce str (interpose " " messages))))
    ;;          message))
    ;;      ";\n")
    ))




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




(defn send-tcp
  [obj-msg host port]
  ;;   ex. (send-tcp {:a 2 :b [2]}  :localhost 3000)
  (try
    (let [socket (new Socket (name host) port)
          out (.getOutputStream socket)
          dos (new DataOutputStream out)
          messages (dict->fudi-str-seq obj-msg)
          ;;_ (prn messages)
          buffer (.getBytes (reduce str messages))
          ]
      (.write dos buffer 0 (alength buffer)))
    (catch Exception e (.printStackTrace e))))


;; (defn check-tcp-connection [host port]
;;   (try
;;     (with-open [socket (new Socket host port)]
;;       (.isConnected socket))
;;     (catch Exception e (.printStackTrace e))))

;; (comment
;;   (defn check-tcp-response [host port]
;;     (with-open [sock (Socket. host port)
;;                 writer (io/writer sock)
;;                 reader (io/reader sock)
;;                 response (StringWriter.)]
;;       (doto writer (.append "isConnected? bang;") (.flush))
;;       (io/copy reader response)
;;       (str response)))
;;   (check-tcp-response "localhost" 3000))

;; (check-tcp-connection "localhost" 3000)



(defn send-fudi [message port]
  (with-open [sock (new Socket "localhost" port)
              writer (io/writer sock)]
    (doto writer
      (.append message)
      (.flush))))
