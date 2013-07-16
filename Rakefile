task :default => [:setup]

desc "Clean the stuff we build"
task :clean do |t|
  rm_rf assets/bootstrap
  rm_rf assets/bootstrap.zip
end

desc "Build stuff we don't want to commit."
task :setup do |t|
  system "curl http://twitter.github.io/bootstrap/assets/bootstrap.zip -o assets/bootstrap.zip"
  system "cd assets ; unzip bootstrap.zip"
end
