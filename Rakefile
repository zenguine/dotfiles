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
    target = "#{ENV["HOME"]}/.#{file}"

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
      `mv "$HOME/.#{file}" "$HOME/.#{file}.backup"` if backup || backup_all
    end
    `ln -s "$PWD/#{link_file}" "#{target}"` unless skip_all
  end

  # Update vim plugin git submodules
  `git submodule init`
  `git submodule update`

  # Additional setup for vim command-t plugin
  Dir.chdir 'vim.linkme/bundle/command-t/ruby/command-t'
  `ruby extconf.rb`
  `rake make`

end
