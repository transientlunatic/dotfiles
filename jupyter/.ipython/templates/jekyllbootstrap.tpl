{% extends 'markdown.tpl' %}

{%- block header -%}
---
layout: ipython
title: "{{resources['metadata']['name']}}"
language: "{{nb.metadata['kernelspec']['language']}}"
tags:
    - notebook
---
{%- endblock header -%}

{% block in_prompt %}
<div class="row">
<!--
<div class="col-md-1">
<span class="label label-info">
In [{{ cell.execution_count }}]:
</span>
</div>
-->
{% endblock in_prompt %}

{% block input %}
<div class="col-md-12">
{{ '{% highlight python %}' }}
{{ cell.source }}
{{ '{% endhighlight %}' }}
</div>
</div>
{% endblock input %}


{% block data_svg %} 
<div class="col-md-12">
<img src="{{ output.metadata.filenames['image/svg'] | path2support }}" alt='svg' />
</div>

{% endblock data_svg %} 

{% block data_png %} 
<div class="col-md-12">
<img src="{{ output.metadata.filenames['image/png'] | path2support }}" alt='png' />
</div>
{% endblock data_png %} 

{% block data_jpg %} 
<div class="col-md-12">
<img src="{{ output.metadata.filenames['image/jpg'] | path2support }}" alt='jpg' />
</div>

{% endblock data_jpg %} 

{% block markdowncell scoped %} 
{{ cell.source  }} 
{% endblock markdowncell %} 

{% block headingcell scoped %}
{{ '#' * cell.level }} {{ cell.source | replace('\n', ' ') }}
{% endblock headingcell %}


{% block stream %}
<div class="col-md-12">
<pre>
 {{ output.text }}
</pre>
</div>
{% endblock stream %}
