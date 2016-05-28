{:user
 {:plugins
  [[jonase/eastwood "0.2.3"]
   [lein-ancient "0.6.10"]
   [lein-create-template "0.2.0"]
   [lein-maven-s3-wagon "0.2.5"]
   [lein-midje "3.2"]
   [lein-ring "0.9.7"]
   [lein-try "0.4.3"]]}
 :repl {:plugins [[cider/cider-nrepl "0.12.0"]]}
 :auth
 {:repository-auth
  {#"s3(p)?:\/\/rk-maven"
   {:username :env/aws_access_key_id
    :passphrase :env/aws_secret_key}}}}
