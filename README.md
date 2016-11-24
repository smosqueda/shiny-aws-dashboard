# shiny-aws-dashboard
This is a shiny app (R, bootstrap js and css combination). The UI gives the user a choice of server groupings in Amazon. Then they can choose several servers and up to 4 metrics. Click the "Plot" button to have the information graph. 

The information for the graphs comes from another project - a Mule/Java service. The services are comprised of CloudWatch metrics using Amazons CloudWatch API in Java. In the beginning, the data came from python scripts that called Amazon's CloudWatch with python and the data was saved to a MySQL database. To make the project more "permanent" a Java service was made instead. 

There is a csv file to identify the AWS server instance names and their server grouping. This file is read to create the drop-down menu of server offerings. 
