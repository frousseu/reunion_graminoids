

library(jsonlite)
library(readxl)

#x<-fromJSON("https://api.inaturalist.org/v1/observations/90513306")
#x$results$observation_photos[[1]]$photo$attribution

d<-as.data.frame(read_excel("C:/Users/God/Documents/reunion_graminoids/grasses.xlsx"))
dcsv<-read.csv("C:/Users/God/Documents/reunion_graminoids/grasses.csv",sep=";")
d<-merge(d,dcsv[,c("sp","rank","attribution")],all.x=TRUE) # only get attributions
d<-d[order(d$sp,d$rank),]

d$photo<-gsub("/medium.|/small.","/large.",d$photo)
#write.table(d[,1:9],"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")

d$idphoto<-lapply(strsplit(sapply(strsplit(d$photo,"/large."),function(i){if(length(i)==1){NA}else{i[1]}}),"/"),tail,1)
d$idobs<-ifelse(!is.na(d$idphoto),sapply(strsplit(d$obs,"/"),tail,1),NA)
  
### only get attributions for empty ones
#w<-1:nrow(d) # get them all to verify if any attributions have changed
w<-which(!is.na(d$idphoto) & is.na(d$attribution))
for(i in w){ # looping is better cause sometimes it times-out
  if(is.na(d$idobs[i])){
    a<-d$credit[i]
  }else{  
    x<-fromJSON(paste0("https://api.inaturalist.org/v1/observations/",d$idobs[i]))
    m<-match(d$idphoto[i],x$results$observation_photos[[1]]$photo$id)
    a<-x$results$observation_photos[[1]]$photo$attribution[m]
  }
  d$attribution[i]<-a
  Sys.sleep(1)
  cat("\r",paste(match(i,w),length(w),sep=" / "))
}

#d<-d[,!sapply(d,class)=="list"]

write.table(d,"C:/Users/God/Documents/reunion_graminoids/grasses.csv",row.names=FALSE,sep=";",na="")


d$flore<-ifelse(is.na(d$flore),"",d$flore)


#d<-d[order(as.integer(is.na(d$photo)),-as.integer(factor(d$family)),d$sp,d$rank),]
d<-d[order(-as.integer(factor(d$family)),d$sp,d$rank),]



css<-function(i){
cat("
<!DOCTYPE html>
  <html>
  <head>
  <link href='https://fonts.googleapis.com/css?family=Roboto Mono' rel='stylesheet'>
  <style>
  body .main-container {
    max-width: 1950px !important;
    width: 1950px !important;
  }
body {
  max-width: 1950px !important;
}
* {
  box-sizing: border-box;
}
p {
  padding: 0px;
  margin: 4px;
  font-family: 'Roboto Mono';
}
.flore {
  color: #FFFFFF77;
    font-style: italic;
}
.column {
  float: left;
  width: 33%;
  padding: 8px;
  background: red;
}
.species {
  width: 100%;
  padding: 0px;
  background: forestgreen;
}
.row::after {
  content: \"\";
  clear: both;
  display: table;
  background: blue;
}
.img2 {
  height:214px;
  width:214px;
  object-fit:cover;
  padding: 2px;
  background: #EEEEEE;
  background-origin: content-box;
}
.img2:hover {
  opacity: 0.70;
  filter: alpha(opacity=100);
}
.p2 {
  color:#FFFFFFEE;
  font-size:30px;
  font-family:'Helvetica' 
}
#img2 {
  border-radius: 5px;
  cursor: pointer;
  transition: 0.3s;
}

/* The Modal (background) */
.modal {
display: none; /* Hidden by default */
position: fixed; /* Stay in place */
z-index: 1; /* Sit on top */
padding-top: 20px; /* Location of the box */
left: 0;
top: 0;
width: 100%; /* Full width */
height: 100%; /* Full height */
overflow: auto; /* Enable scroll if needed */
background-color: rgb(0,0,0); /* Fallback color */
background-color: rgba(0,0,0,0.9); /* Black w/ opacity */
}

/* Modal Content (image) */
.modal-content {
margin: auto;
display: block;
width: 80%;
max-width: 700px;
}

/* Caption of Modal Image */
#caption {
margin: auto;
display: block;
width: 80%;
max-width: 900px;
text-align: center;
color: #ccc;
padding: 10px 0;
height: 150px;
}

/* Add Animation */
.modal-content, #caption {  
-webkit-animation-name: zoom;
-webkit-animation-duration: 0.3s;
animation-name: zoom;
animation-duration: 0.3s;
}

@-webkit-keyframes zoom {
from {-webkit-transform:scale(0)} 
to {-webkit-transform:scale(1)}
}

@keyframes zoom {
from {transform:scale(0)} 
to {transform:scale(1)}
}

/* The Close Button */
.close {
position: absolute;
top: 15px;
right: 35px;
color: #f1f1f1;
font-size: 40px;
font-weight: bold;
transition: 0.3s;
}

.close:hover,
.close:focus {
color: #bbb;
text-decoration: none;
cursor: pointer;
}

/* 100% Image Width on Smaller Screens */
@media only screen and (max-width: 700px){
.modal-content {
width: 100%;
}
}
</style>
  </head>
  <body>
  <div class=\"species\">
  <h2 style = \"color:#FFFFFFEE;font-size:40px;font-family:'Helvetica';\">Index photographique des graminées de la Réunion</h2>
  </div>
  <p style = \"color:black;font-size:22px;\">Cette page est un index photographique des graminées de la Réunion. La liste des espèces présentes est basée sur les espèces décrites dans la Flore des Mascareignes et les espèces listéees dans l'index de la flore de la Réunion du CBNM. Les photos proviennent presque toutes d'observations sur iNaturalist. Les photos présentées sont toutes sous une license CC permettant leur utilisation à des fins non-commerciales. Passez votre curseur sur une photo pour voir la license utilisée et l'auteur de a photo et cliquez sur la photo pour voir la photo originale. Pour toutes questions ou commentaires: ici</p>
<div class=\"species\">
<p style = \"color:#FFFFFFEE;font-size:30px;\">
Nom dans iNaturalist <span class=\"flore\">Nom dans la Flore des Mascareignes</span><span style=\"float:right;\">Famille</span>
</p>
</div>
")

}  
  
species_header<-function(x,i){
  cat(paste0(
  "<div class=\"species\">
    <p class=\"p2\">
      ",x$sp[i]," <span class=\"flore\">",x$flore[i],"</span>"," <span style=\"float:right;\">",x$family[i],"</span>
    </p>
  </div>  
  "))
}

species_photo<-function(x,i){
  cat(paste0(
    "<img class=\"img2\" src=\"",x$photo[i],"\" src2=\"",gsub("large.","small.",x$photo[i]),"\" title=\"",x$attribution[i],"\">"
  ))
}

l<-split(d,factor(d$sp,levels=unique(d$sp)))


con <- file("C:/Users/God/Downloads/index.html", open = "wt", encoding = "UTF-8")
sink(con)
css()
invisible(
  lapply(l,function(i){
    species_header(i,1)
    if(!is.na(i$photo[1])){
      invisible(
        sapply(1:nrow(i),function(j){species_photo(i,j)})
      )
    }
  })
)
cat("
<div id=\"myModal\" class=\"modal\">
  <span class=\"close\">&times;</span>
  <img class=\"modal-content\" id=\"img01\">
  <div id=\"caption\"></div>
</div>     
")
cat("
<script>

    // create references to the modal...
    var modal = document.getElementById('myModal');
    // to all images -- note I'm using a class!
    var images = document.getElementsByClassName('img2');
    // the image in the modal
    var modalImg = document.getElementById(\"img01\");
    // and the caption in the modal
    var captionText = document.getElementById(\"caption\");
    
    modal.addEventListener('click',function(){
    this.style.display=\"none\";
    })
    
    // Go through all of the images with our custom class
    for (var i = 0; i < images.length; i++) {
    var img = images[i];
    // and attach our click listener for this image.
    img.onclick = function(evt) {
    console.log(evt);
    modal.style.display = \"block\";
    modalImg.src = this.src;
    captionText.innerHTML = this.title;
    }
    }
    
    var span = document.getElementsByClassName(\"close\")[0];
    
    span.onclick = function() {
    modal.style.display = \"none\";
    }
    
    </script>    
")
cat("
 </body>
 </html>
")
sink()
close(con)

file.show("C:/Users/God/Downloads/index.html")

