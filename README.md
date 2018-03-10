## About Toplists
This is a small web application that lets users create lists of items. Other people can then sort these lists according to their own rankings and compare them.

## Module description

```
TOPLISTS is a radiance module:
  Domain: "toplists"
  Implements: Nothing
  Claimed Pages: #<URI-DISPATCHER VIEW-ORDER #@"toplists/^([^/]+)/([^/]+)$">,
                 #<URI-DISPATCHER NEW-ORDER #@"toplists/^([^/]+)/order$">,
                 #<URI-DISPATCHER EDIT-LIST #@"toplists/^([^/]+)/edit$">,
                 #<URI-DISPATCHER VIEW-LIST #@"toplists/^([^/]+)$">,
                 #<URI-DISPATCHER NEW-LIST #@"toplists/^new$">,
                 #<URI-DISPATCHER ALL #@"toplists/^$">
  API Endpoints: toplists/order/delete, toplists/order/edit,
                 toplists/order/create, toplists/order/view,
                 toplists/list/delete, toplists/list/edit,
                 toplists/list/create, toplists/list/view
  Configuration: :PERMISSIONS
  Permissions: toplists.order, toplists.delete.own, toplists.edit.own,
               toplists.view, toplists.create
  Hooks: LIST-CREATED, LIST-UPDATED, LIST-DELETED, ORDER-CREATED,
         ORDER-UPDATED, ORDER-DELETED
```
