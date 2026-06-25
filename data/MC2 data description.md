# MC2 Data description

This folder contains the data for the anomalous postings originating from Tenant Thread. 

This data is based on the actions taken by the employees and personal agents inside of Tenant Thread.  

## Key event:

The anomalous posting is sent to SaidIt by John Windward on May 17, 2046 at 4:21am.

## Files

MC2_Data.json -- A timeline of events gathered by the internal systems.
org_chart.json -- The organization chart of Tenant Thread including people and departments. Compatible with networkX node_link_graph.


## Schema

Derived from data:

```json
{
   "description": "string",
   "events": [
      {
         "short_name": "string",
         "parties": "array",
         "when": "float",
         "details": "object",
         "id": "int"
      }
   ]
}
```

## Data Description

The data is structured as a timeline.  The events are ordered and indicate the operations 
performed by people, autonomous agents and office systems.

Field meanings:
- short_name: The action taken
- parties: Which people/agents/systems were involved.
- when: Unix timestamp.  The challenge occurs in May 2046, and timestamps should be in that month.
- details: Additional information about the action taken.  The contents vary by the contents of short_name.
- id: A unique id for each record. 
