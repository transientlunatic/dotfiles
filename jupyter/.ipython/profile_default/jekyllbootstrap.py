# modification of config created here: https://gist.github.com/cscorley/9144544

from time import gmtime, strftime
datestr = strftime("%Y-%m-%d", gmtime())

try:
    from urllib.parse import quote  # Py 3
except ImportError:
    from urllib2 import quote  # Py 2
import os
import sys

f = None
for arg in sys.argv:
    if arg.endswith('.ipynb'):
        f = arg.split('.ipynb')[0]
        break


c = get_config()
c.NbConvertApp.export_format = 'markdown'
c.MarkdownExporter.template_path = ['/home/daniel/.ipython/templates'] # point this to your jekyll template file
c.MarkdownExporter.template_file = 'jekyllbootstrap.tpl'
#c.Application.verbose_crash=True

# modify this function to point your images to a custom path
# by default this saves all images to a directory 'images' in the root of the blog directory
def path2support(path):
    """Turn a file path into a URL"""
    return '{{ site.baseurl }}/images/'+str(datestr)+"/"+ os.path.basename(path)

c.MarkdownExporter.filters = {'path2support': path2support}

if f:
    c.NbConvertApp.output_base = f.lower().replace(' ', '-')
    #c.FilesWriter.build_directory = '/home/daniel/website/notebook/notebooks' # point this to your build directory
