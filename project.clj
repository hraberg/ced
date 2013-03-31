(defproject ced "0.1.0-SNAPSHOT"
  :description "CED - Ced is the standard Clojure EDitor."
  :url "http://www.gnu.org/software/ed/"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [xtc/rats-runtime "2.3.1"]
                 [com.nativelibs4java/anarres-jnaerator "1.2.8"
                  :exclusions [commons-collections/commons-collections]]]
  :profiles {:dev {:dependencies [[xtc/rats "2.3.1"]]}}
  :plugins [[lein-swank "1.4.5"]]
  :resource-paths ["ed-1.7" "resources"]
  :jar-exclusions [#".o" #"doc/.*" #"ChangeLog.*" #"Makefile.*" #"\p{Upper}+"]
  :java-source-paths ["src"]
  :main ced.main)