<div class="editorial-list">
<% let n = 0; for (const item of items) { n++; %>
<div class="editorial-item">
<div class="editorial-num"><%= String(n).padStart(2, '0') %></div>
<div class="editorial-body">
<a class="editorial-title" href="<%- item.path %>"><%= item.title %></a>
<% if (item.description) { %><div class="editorial-desc"><%= item.description %></div><% } %>
<% if (item.script) { %><div class="editorial-script"><a href="<%- item.script %>">R script</a></div><% } %>
</div>
</div>
<% } %>
</div>
