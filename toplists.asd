#|
 This file is a part of Toplists
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:toplists
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :version "1.0.1"
  :description "A top list ranking application for Radiance."
  :homepage "https://Shirakumo.github.io/toplists/"
  :bug-tracker "https://github.com/Shirakumo/toplists/issues"
  :source-control (:git "https://github.com/Shirakumo/toplists.git")
  :components ((:file "module")
               (:file "objects")
               (:file "frontend")
               (:file "api"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :profile)
               (:interface :cache)
               :r-data-model
               :r-clip))
