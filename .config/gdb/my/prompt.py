# TODO: Conditional on whether terminal supports colors. If not, define these as ""
#
[grey, white, no_color] = [gdb.prompt.substitute_prompt(c) for c in [
    r"\[\e[00;37m\]",
    r"\[\e[01;37m\]",
    r"\[\e[00m\]",
]]


def my_prompt_hook(_current_prompt):
    try:
        frame = gdb.selected_frame()
    except:
        frame = None

    thread = gdb.selected_thread()

    frame_info = ("[Frame: {} (#{}) ({})]".format(frame.name(), frame.level(), frame.language())
                  if frame else "")
    thread_info = ("{}[Thread: {} (#{})]".format(" " if frame else "", thread.name, thread.num)
                   if thread else "")

    return "\n{}{}{}{}{}(gdb){} ".format(grey, frame_info, thread_info,
                                         "\n" if frame or thread else "",
                                         white, no_color)


gdb.prompt_hook = my_prompt_hook
