"""Run doctest on stdin.

Usage:

    python[23] flycheck-doctest.py --original-file-name /path/to/file.py < /path/to/file.py

The first argument is useful if file.py makes relative imports.
"""

import argparse
import doctest
import imp
import sys
import traceback
from os import path
from xml.etree import ElementTree as et

def load(fpath, code):
    dirname, fname = path.split(fpath)
    modname, _ = path.splitext(fname)

    sys.path.insert(0, dirname)
    mod = imp.new_module(modname)
    setattr(mod, "__file__", fpath)

    # Compile separately to set the right filename (__file__ isn't enough)
    # FIXME catch errors
    bytecode = compile(code, fpath, 'exec')
    # Two levels of exec so the code can be run from both version of Python
    if sys.version_info[0] > 2:
        exec("exec(bytecode, mod.__dict__)")
    else:
        exec("exec bytecode in mod.__dict__")

    sys.modules[modname] = mod

    return mod

class CheckstyleRunner(doctest.DocTestRunner,object):
    """A variant of DocTestRunner that uses Checkstyle format."""

    def __init__(self, *args, **kwargs):
        super(CheckstyleRunner, self).__init__(*args, **kwargs)
        self.checkstyle_errors = []

    def report_start(self, *args):
        pass

    def report_success(self, *args):
        pass

    @staticmethod
    def error_node(test, example, header, message):
        lineno = (test.lineno or 0) + (example.lineno or 0) + 1
        header = (header + ":\n").format(test.name or "<stdin>")
        return make_checkstyle_node(header + message, line=lineno)

    def report_failure(self, out, test, example, got):
        """Report that the given example failed."""
        header = "Incorrect output in {}"
        message = self._checker.output_difference(example, got, self.optionflags)
        err = CheckstyleRunner.error_node(test, example, header, message)
        self.checkstyle_errors.append(err)

    def report_unexpected_exception(self, out, test, example, exc_info):
        """Report that the given example raised an unexpected exception."""
        header = 'Exception raised in {}'
        backtrace = doctest._indent(doctest._exception_traceback(exc_info))
        err = CheckstyleRunner.error_node(test, example, header, backtrace)
        self.checkstyle_errors.append(err)

def make_checkstyle_node(message, line):
    if hasattr(message, "decode"): # Python 2
        message = message.decode('utf-8')
    attr = {"line": str(line), "severity": "error", "message": message}
    return et.Element("error", attr)

def make_checkstyle_tree(fpath, errors):
    root = et.Element("checkstyle")
    fnode = et.SubElement(root, "file", {"name": fpath})
    fnode.extend(errors)
    return root

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--original-file-name")
    parser.add_argument("source", nargs="?", default='-')
    return parser.parse_args()

def check(source_file, original_file_name):
    try:
        if source_file == "-":
            source = sys.stdin.read()
        else:
            with open(source_file, 'U') as f:
                source = f.read()
        mod = load(original_file_name, source)
        runner = CheckstyleRunner(optionflags=doctest.NORMALIZE_WHITESPACE)
        for test in doctest.DocTestFinder().find(mod):
            runner.run(test)
        return runner.checkstyle_errors
    except SyntaxError as e:
        message = "SyntaxError: {}".format(e.msg)
        return [make_checkstyle_node(message, line=e.lineno)]
    except: # pylint: disable=bare-except
        message = "Could not load sources:\n{}".format(traceback.format_exc())
        return [make_checkstyle_node(message, line=0)]

def main():
    args = parse_args()
    orig_fname = args.original_file_name
    tree = make_checkstyle_tree(orig_fname, check(args.source, orig_fname))
    output = et.tostring(tree, encoding="utf-8").decode("utf-8") # Python 2
    sys.stdout.write(output)

main()
