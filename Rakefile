desc "boostrap all files by linking them to their correct locations"
task :bootstrap do
  s_link("zshrc", "~/.zshrc")
  s_link("joshproehl.zsh-theme", "~/.oh-my-zsh/themes/joshproehl.zsh-theme")
end


def s_link(source_path, target_path)
  target_file = File.expand_path(target_path)

  if File.exists?(target_file) and not File.symlink?(target_file)
    puts "Stashing your previous #{target_path} as #{target_path}.old"
    mv(target_file, File.expand_path("#{target_path}.old"))
  end

  unless File.exists?(target_file)
    # Counts on the fact that ln_s outputs "puts" the command for the user to see.
    ln_s(File.expand_path(source_path), target_file)
  end
end
