# Main

## Mongo db setup

```bash
docker run -d --name some-mongo -p 27017:27017 mongo
```

## Run
```bash
docker start some-mongo # start database container
export VK_TOKEN=token
iex -S mix
> Main.mongo
```

## Check results
```bash
docker exec -it some-mongo mongo
> use vk
> db.users.find()
{ "_id" : ObjectId("61135acac68a83477ce82cfb"), "can_access_closed" : true, "first_name" : "Anna", "id" : 11775987, "is_closed" : false, "last_name" : "Gayterova" }
{ "_id" : ObjectId("61135acac68a83477ce82cfc"), "can_access_closed" : false, "first_name" : "Grigory", "id" : 14016611, "is_closed" : true, "last_name" : "Revyagin" }
{ "_id" : ObjectId("61135acac68a83477ce82cfd"), "can_access_closed" : true, "first_name" : "Stanislav", "id" : 16047434, "is_closed" : false, "last_name" : "Salko" }
{ "_id" : ObjectId("61135acac68a83477ce82cfe"), "can_access_closed" : true, "first_name" : "Tatyana", "id" : 16550398, "is_closed" : false, "last_name" : "Lukasevich" }
{ "_id" : ObjectId("61135acac68a83477ce82cff"), "can_access_closed" : true, "first_name" : "Irina", "id" : 18018866, "is_closed" : false, "last_name" : "Chulkova" }
```
