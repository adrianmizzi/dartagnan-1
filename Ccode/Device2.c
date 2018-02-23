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
PROCESS(process_2, "Process 2");
PROCESS(process_3, "Process 3");
PROCESS(process_5, "Process 5");
PROCESS(process_9, "Process 9");
AUTOSTART_PROCESSES(&init_process, &scheduler_process);
/*---------------------------------------------------------------------------*/

static process_event_t event_data_ready_2;


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
    case 2 :
      process_start(&process_2, NULL);
      break;
    case 3 :
      process_start(&process_3, NULL);
      break;
    case 5 :
      process_start(&process_5, NULL);
      break;
    case 9 :
      process_start(&process_9, NULL);
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
  event_data_ready_2 = process_alloc_event();

  init_comms(&unicast_callbacks);

  // schedule jobs in scheduler


  PROCESS_END();
}

PROCESS_THREAD(process_2, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x137,x155 = 0;
  x155 = (int) readTemperature();
  char result[5];
  itoa(x155, result, 10);
  char *message = encode(2, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_3, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x136,x154 = 0;
  x154 = (int) readTemperature();
  char result[5];
  itoa(x154, result, 10);
  char *message = encode(3, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_5, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x138,x156 = 0;
  x156 = (int) readTemperature();
  char result[5];
  itoa(x156, result, 10);
  char *message = encode(5, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_9, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x139,x157 = 0;
  x157 = (int) readTemperature();
  char result[5];
  itoa(x157, result, 10);
  char *message = encode(9, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}
