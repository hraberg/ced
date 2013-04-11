(defproject ced "0.1.0-SNAPSHOT"
  :description "CED - C, Clojure Editon."
  :url "https://github.com/hraberg/ced"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.0.0"]
                 [org.flatland/ordered "1.5.1"]
                 [xtc/rats-runtime "2.3.1"]
                 [com.nativelibs4java/anarres-jnaerator "1.2.8"
                  :exclusions [commons-collections/commons-collections
                               gnu.getopt/java-getopt]]]
  :profiles {:dev {:dependencies [[xtc/rats "2.3.1"]
                                  [org.clojure/tools.trace "0.7.5"]]}}
  :plugins [[lein-swank "1.4.5"]]
  :resource-paths ["resources"]
  :java-source-paths ["src"]
  :main ced.main)