import sublime
import sublime_plugin
import subprocess
import tempfile
import re


def call(cmd, args, stdin_content):
    stdin = tempfile.TemporaryFile()
    stdin.write(stdin_content.encode('utf-8'))
    stdin.seek(0)
    stdout = tempfile.TemporaryFile()
    print("running: ", [cmd] + args)
    subprocess.call(args=[cmd] + args, stdin=stdin, stdout=stdout)
    stdout.seek(0)
    return stdout.read().decode('utf-8')[:-1]


def grow_region(view, region):
    start = view.line(region.a).a
    end = view.line(region.b).b
    file_end = view.size()
    while start:
        line_start = view.line(start - 1).a
        line = view.substr(view.line(line_start)).strip()
        if not line:
            break
        start = line_start
    while end < file_end:
        line_end = view.line(end + 1).b
        line = view.substr(view.line(line_end)).strip()
        if not line:
            break
        end = line_end
    return sublime.Region(start, end)


class ReplaceSelectionCommand(sublime_plugin.TextCommand):
    def run(self, edit, cmd=None, args=None):
        if not cmd:
            raise Exception("No command given")
        args = args or []
        view = self.view
        for region in view.sel():
            if not region:
                region = grow_region(view, region)
            content = view.substr(region)
            replacement = call(cmd, args, content)
            view.replace(edit, region, replacement)


WS_RE = re.compile("(\s*)")


class InsertPdbTraceCommand(sublime_plugin.TextCommand):

    _pdb_string = "import pdb; pdb.set_trace()  #miv-debug"

    def run(self, edit, insert_str=None):
        insert_str = insert_str or self._pdb_string
        view = self.view
        sel = view.sel()
        new_sel = []
        for region in sel:
            point = region.a
            line = view.full_line(point)
            line_content = view.substr(line)
            if line_content.strip() == insert_str:
                view.replace(edit, line, "")
                new_sel.append(sublime.Region(point, point))
            else:
                actual_insert_str = insert_str
                insert_point = point
                if line_content.strip():
                    insert_point = line.a
                    indent = WS_RE.match(line_content).groups()[0]
                    actual_insert_str = indent + insert_str + "\n"
                nchars = view.insert(edit, insert_point, actual_insert_str)
                new_sel.append(sublime.Region(
                    insert_point, insert_point + nchars))
        sel.clear()
        sel.add_all(new_sel)
