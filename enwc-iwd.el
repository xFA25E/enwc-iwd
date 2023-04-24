;;; enwc-iwd.el --- IWD backend for ENWC  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>
;; URL: https://github.com/xFA25E/enwc-iwd
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (enwc "2.0"))
;; Keywords: comm

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; IWD[1] backend for ENWC[2].

;;;; Usage

;; Set `enwc-default-backend' to iwd.

;;;; Installation

;;;;; Package manager

;; If you've installed it with your package manager, you're done.

;;;;; Manual

;; Install these required packages:

;; + enwc

;; Then put this file in your load-path.

;;;; Credits

;; This package would not have been possible without the enwc[2] package.

;;  [1] https://iwd.wiki.kernel.org
;;  [2] https://savannah.nongnu.org/p/enwc

;;; Code:

;;;; Requirements

(require 'dbus)
(require 'enwc)

;;;; Variables

(defvar i--dbus-service "net.connman.iwd"
  "IWD D-Bus service.")

(defvar i--dbus-device-interface (concat i--dbus-service ".Device")
  "IWD D-Bus device interface.")

(defvar i--dbus-network-interface (concat i--dbus-service ".Network")
  "IWD D-Bus network interface.")

(defvar i--dbus-station-interface (concat i--dbus-service ".Station")
  "IWD D-Bus station interface.")

(defvar i--dbus-path "/net/connman/iwd"
  "IWD D-Bus prefix path.")

(defvar i--dbus-device-path nil
  "IWD D-Bus device path.")

(defvar i--dbus-properties-changed-signal nil
  "D-Bus signal object for PropertiesChanged signal.")

(defvar i--is-connecting-p nil
  "IWD connecting state.")

(defvar i--current-nw-id nil
  "IWD connected network.")

(defvar i--networks (cons nil (make-hash-table :test 'equal))
  "A cons cell with current IWD networks.
Car is a list with D-Bus paths.  It is ordered in a particular
way for ENWC.  Used as a fast cache in `enwc-iwd-network-ids'
call.

Cdr is a hash table where keys are D-Bus paths and values are network
properties.  Used as a fast cache in `enwc-iwd-nw-props'.")

;;;; Functions

;;;;; Public

;;;;;; Backend

;;;;;;; Loading/unloading

(defun i-can-load-p ()
  "Check whether IWD service is available."
  (member i--dbus-service (dbus-list-known-names :system)))

(defun i-load ()
  "Setup IWD backend.
Find device D-Bus path.  Register signal for properties.  Get
current state.  Get current connected network."
  (setq i--dbus-device-path (i--dbus-device-path)
        i--dbus-properties-changed-signal (i--dbus-properties-changed-signal)
        i--is-connecting-p (i--is-connecting-p)
        i--current-nw-id (i--current-nw-id))
  (i--set-networks))

(defun i-unload ()
  "Unregister D-Bus signal for properties."
  (dbus-unregister-object i--dbus-properties-changed-signal))

;;;;;;; Scan interface

(defun i-network-ids (&optional wiredp)
  "Get a list of available IWD networks.
Network is represented by a D-Bus object path.  If WIREDP is
non-nil, do nothing and return nil."
  (unless wiredp
    (car i--networks)))

(defun i-scan ()
  "Call IWD scan method."
  (dbus-call-method :system
                    i--dbus-service
                    i--dbus-device-path
                    i--dbus-station-interface
                    "Scan"))

(defun i-nw-props (path &optional wiredp)
  "Get network properties for IWD network PATH.
PATH is a D-Bus object path.  If WIREDP is non-nil, do nothing."
  (unless wiredp
    (gethash path (cdr i--networks))))

;;;;;;; Connect/disconnect

(defun i-connect (path &optional wiredp)
  "Connect to IWD network with PATH.
PATH is a D-Bus object path.  If WIREDP is non-nil, do nothing."
  (unless wiredp
    (dbus-call-method :system
                      i--dbus-service
                      path
                      i--dbus-network-interface
                      "Connect")))

(defun i-disconnect (&optional wiredp)
  "Disconnect from currently connected network in IWD.
If WIREDP is non-nil, do nothing."
  (unless wiredp
    (dbus-call-method :system
                      i--dbus-service
                      i--dbus-device-path
                      i--dbus-station-interface
                      "Disconnect")))

;;;;;;; Maintenance

(defun i-current-nw-id (&optional wiredp)
  "Get current IWD connected network.
When WIREDP is non-nil, return nil."
  (unless wiredp
    i--current-nw-id))

(defun i-is-connecting-p ()
  "Check whether IWD is in connecting state."
  i--is-connecting-p)

(defun i-is-wired-p ()
  "IWD is not a wired backend."
  nil)

;;;;; Private

(defun i--strength-from-signal (signal)
  "Convert SIGNAL to strength percantage."
  ;; From iwgtk.
  (cond ((< -6000 signal) 100)
        ((< -6700 signal) 75)
        ((< -7400 signal) 50)
        ((< -8100 signal) 25)
        (t 0)))

(defun i--dbus-device-path ()
  "Get D-Bus path for current wireless device."
  (cl-loop
   for path in (dbus-introspect-get-all-nodes :system
                                              i--dbus-service
                                              i--dbus-path)
   for interfaces = (dbus-introspect-get-interface-names :system
                                                         i--dbus-service
                                                         path)
   when (and (member i--dbus-device-interface interfaces)
             (string= enwc-wireless-device
                      (dbus-get-property :system
                                         i--dbus-service
                                         path
                                         i--dbus-device-interface
                                         "Name")))
   return path))

(defun i--dbus-properties-changed-signal ()
  "Get D-Bus signal for PropertiesChanged."
  (dbus-register-signal :system
                        i--dbus-service
                        i--dbus-device-path
                        "org.freedesktop.DBus.Properties"
                        "PropertiesChanged"
                        #'i--dbus-properties-changed-signal-handler))

(defun i--is-connecting-p ()
  "Get IWD connecting state."
  (string= "connecting" (dbus-get-property :system
                                           i--dbus-service
                                           i--dbus-device-path
                                           i--dbus-station-interface
                                           "State")))

(defun i--current-nw-id ()
  "Get IWD connected network."
  (ignore-errors (dbus-get-property :system
                                    i--dbus-service
                                    i--dbus-device-path
                                    i--dbus-station-interface
                                    "ConnectedNetwork")))

(defun i--set-networks ()
  "Set variable `enwc-iwd--networks'.
See its documentation string.  Return its value."
  (cl-loop initially (setcar i--networks nil)
           with networks = (clrhash (cdr i--networks))

           for (path signal) in (dbus-call-method :system
                                                  i--dbus-service
                                                  i--dbus-device-path
                                                  i--dbus-station-interface
                                                  "GetOrderedNetworks")

           for properties = (dbus-get-all-properties :system
                                                     i--dbus-service
                                                     path
                                                     i--dbus-network-interface)
           do (puthash
               path
               `((strength . ,(i--strength-from-signal signal))
                 (essid . ,(cdr (assoc "Name" properties)))
                 (encrypt . ,(cdr (assoc "Type" properties)))
                 (bssid . "")
                 (channel . 0))
               networks)

           collect path into ordered-networks

           finally do (setcar i--networks ordered-networks)
           finally return i--networks))

(defun i--dbus-properties-changed-signal-handler (interface changed invalidated)
  "Handler for PropertiesChanged signal.
For INTERFACE, CHANGED and INVALIDATED look for PropertiesChanged
signal in D-Bus documentation."
  (when (string= i--dbus-station-interface interface)
    (dolist (property-with-values changed)
      (pcase property-with-values
        (`("Scanning" (nil . ,_))
         (i--set-networks)
         (enwc-process-scan))

        (`("State" (,value . ,_))
         (message "(ENWC) IWD State: %s" value)
         (cond ((string= "connecting" value)
                (setq i--is-connecting-p t))
               (i--is-connecting-p
                (setq i--is-connecting-p nil))))

        (`("ConnectedNetwork" (,network . ,_))
         (setq i--current-nw-id network))))

    (dolist (property invalidated)
      (when (string= "ConnectedNetwork" property)
        (setq i--current-nw-id nil)))))

;;;; Footer

(enwc-register-backend
 (make-enwc-backend
  ;; The symbol that identifies this backend.
  :key 'iwd
  ;; Loading/unloading functions
  :can-load-p #'i-can-load-p
  :load #'i-load
  :unload #'i-unload
  ;; Scan interface
  :network-ids #'i-network-ids
  :scan #'i-scan
  :nw-props #'i-nw-props
  ;; Connect/disconnect
  :connect #'i-connect
  :disconnect #'i-disconnect
  ;; Maintenance
  :current-nw-id #'i-current-nw-id
  :is-connecting-p #'i-is-connecting-p
  :is-wired-p #'i-is-wired-p))

(provide 'enwc-iwd)

;; Local Variables:
;; read-symbol-shorthands: (("i-" . "enwc-iwd-"))
;; End:

;;; enwc-iwd.el ends here
