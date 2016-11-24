{:user
 {:plugins
  [[com.jakemccrary/lein-test-refresh "0.18.0"]
   [lein-ancient "0.6.10"]
   [lein-create-template "0.2.0"]
   [lein-maven-s3-wagon "0.2.5"]
   [lein-midje "3.2.1"]
   [lein-ring "0.10.0"]
   [lein-try "0.4.3"]]
  :repl-options {:timeout 300000}
  :dependencies [[slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :repl {:plugins [[cider/cider-nrepl "0.14.0"]]}
 :auth
 {:repository-auth
  {#"s3(p)?:\/\/rk-maven"
   {:username :env/aws_access_key_id
    :passphrase :env/aws_secret_key}}}}
