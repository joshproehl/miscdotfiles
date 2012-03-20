desc "boostrap all files by linking them to their correct locations"
task :bootstrap do
  puts "AutoLinking all files with _* prefix to their ~/.* location."
  Dir.glob("_*") do |filename|
    newFilename = filename.gsub(/_/, "~/.")
    puts "  - Ensuring #{filename} linked to #{newFilename}"
    s_link(filename, newFilename)
  end
  puts "Done AutoLinking."

  puts "Handling manual stuff..."
  # Install the ruby-build plugin for rbenv only
  if !File.exists?(File.expand_path("~/.rbenv/plugins/"))
    mkdir File.expand_path("~/.rbenv/plugins/")
  end
  s_link("bundle/ruby-build", "~/.rbenv/plugins/ruby-build")
  puts "Done bootstraping! Environment is set up."
end

# Create a symbolic link from the source to the target.
def s_link(source_path, target_path)
  target_file = File.expand_path(target_path)

  # If the target file exists we don't really want to destroy it...
  if File.exists?(target_file) and not File.symlink?(target_file)
    puts "  ! Stashing your previous #{target_path} as #{target_path}.old"
    mv(target_file, File.expand_path("#{target_path}.old"))
  end

  if File.symlink?(target_file) and File.readlink(target_file).to_s != File.expand_path(source_path).to_s
    puts "  ! Existing symlink appears to point to incorrect file, moving it to #{target_file}.old"
    mv(target_file, File.expand_path("#{target_path}.old"))
  end

  # At this point either the target file is the symlink we want,
  # or something weird happened and we don't want to replace it.
  unless File.exists?(target_file)
    # Counts on the fact that ln_s outputs "puts" the command for the user to see.
    ln_s(File.expand_path(source_path), target_file)
  end
end
