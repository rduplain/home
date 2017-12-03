{:user
 {:plugins
  [[com.jakemccrary/lein-test-refresh "0.21.1"]
   [lein-ancient "0.6.14"]
   [lein-cloverage "1.0.10"]
   [lein-create-template "0.2.0"]
   [lein-midje "3.2.1"]
   [lein-ring "0.12.1"]
   [lein-tach "0.4.0"]
   [lein-try "0.4.3"]
   [s3-wagon-private "1.3.1"]]
  :repl-options {:timeout 300000}
  :dependencies [[slamhound "1.5.5"]]
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :repl {:plugins [[cider/cider-nrepl "0.15.1"]]}
 :auth
 {:repository-auth
  {#"s3(p)?:\/\/rk-maven"
   {:username :env/aws_access_key_id
    :passphrase :env/aws_secret_key}}}}
