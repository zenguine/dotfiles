require 'rake'

desc "Symlink configuration files to their correct locations"
task :install do
  link_files = Dir.glob("**/*.linkme")
  skip_all = false
  overwrite_all = false
  backup_all = false

  link_files.each do |link_file|
    overwrite = false
    backup = false
    file = link_file.split("/").last.split(".linkme").last
    if file != 'scripts'
      target = "#{ENV["HOME"]}/.#{file}"
    else
      target = "#{ENV["HOME"]}/#{file}"
    end

    if File.exists?(target) || File.symlink?(target)
      unless skip_all || overwrite_all || backup_all
        puts "File already exists: #{target}, Choose action: [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all"
        case STDIN.gets.chomp
        when 'o' then overwrite = true
        when 'O' then overwrite_all = true
        when 's' then next
        when 'S' then skip_all = true
        when 'b' then backup = true
        when 'B' then backup_all = true
        end
      end
      FileUtils.rm_rf(target) if overwrite || overwrite_all
      `mv #{ENV["HOME"]}/.#{file} #{ENV["HOME"]}/.#{file}.backup` if backup || backup_all
    end
    `ln -s #{ENV["PWD"]}/#{link_file} #{target}` unless skip_all

  end

  # Update vim plugin git submodules
  puts "Retrieving git submodules.."
  `git submodule update --init --recursive`

  # Build source in build/ sub-dir --------
  puts "Building source from 'build' sub-directory"
  
  # Install matcher
  puts "Building: matcher"
  `make -C build/matcher`
  `make -C build/matcher install`

  puts "Installing vim plugins via BundleInstall (with vundle)"
  `vim -c 'BundleInstall' -c 'qa'`

end
