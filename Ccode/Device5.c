#include "contiki.h"
#include "net/rime/rime.h"

#include <stdio.h>
#include <stdbool.h>
#include "../lib/mycomms.c"
#include "../lib/mysensorutils.c"
#include "../lib/myutils.c"
#include "../lib/myencoderdecoder.c"
#include "../lib/myscheduler.c"

/*---------------------------------------------------------------------------*/
PROCESS(init_process, "Initialise Process");
PROCESS(process_13, "Process 13");
PROCESS(process_16, "Process 16");
PROCESS(process_17, "Process 17");
PROCESS(process_19, "Process 19");
AUTOSTART_PROCESSES(&init_process, &scheduler_process);
/*---------------------------------------------------------------------------*/

static process_event_t event_data_ready_13;


static linkaddr_t node1 = { { 21, 59 } };
static linkaddr_t node2 = { { 13, 41 } };
static linkaddr_t node3 = { { 168, 31 } };
static linkaddr_t node4 = { { 207, 37 } };
static linkaddr_t node5 = { { 96, 41 } };

static void recv_uc(struct unicast_conn *c, const linkaddr_t *from)
{
  char *incoming;
  incoming = packetbuf_dataptr();

  printf("Received a message: %s \n", incoming);

  switch (getMessageType(incoming)) {
    case 13 :
      process_start(&process_13, NULL);
      break;
    case 16 :
      process_start(&process_16, NULL);
      break;
    case 17 :
      process_start(&process_17, NULL);
      break;
    case 19 :
      process_start(&process_19, NULL);
      break;
    default :
      break;
  }
}

static const struct unicast_callbacks unicast_callbacks = {recv_uc};
/*---------------------------------------------------------------------------*/

PROCESS_THREAD(init_process, ev, data)
{
  PROCESS_BEGIN();

  initialiseTemperatureSensor();

  // allocate the events
  event_data_ready_13 = process_alloc_event();

  init_comms(&unicast_callbacks);

  // schedule jobs in scheduler


  PROCESS_END();
}

PROCESS_THREAD(process_13, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x145,x163 = 0;
  x163 = (int) readTemperature();
  char result[5];
  itoa(x163, result, 10);
  char *message = encode(13, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_16, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x151,x169 = 0;
  x169 = (int) readTemperature();
  char result[5];
  itoa(x169, result, 10);
  char *message = encode(16, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_17, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x150,x168 = 0;
  x168 = (int) readTemperature();
  char result[5];
  itoa(x168, result, 10);
  char *message = encode(17, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_19, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x152,x170 = 0;
  x170 = (int) readTemperature();
  char result[5];
  itoa(x170, result, 10);
  char *message = encode(19, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}
