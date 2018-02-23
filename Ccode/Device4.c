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
PROCESS(process_8, "Process 8");
PROCESS(process_11, "Process 11");
PROCESS(process_12, "Process 12");
PROCESS(process_15, "Process 15");
PROCESS(process_18, "Process 18");
AUTOSTART_PROCESSES(&init_process, &scheduler_process);
/*---------------------------------------------------------------------------*/

static process_event_t event_data_ready_8;


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
    case 8 :
      process_start(&process_8, NULL);
      break;
    case 11 :
      process_start(&process_11, NULL);
      break;
    case 12 :
      process_start(&process_12, NULL);
      break;
    case 15 :
      process_start(&process_15, NULL);
      break;
    case 18 :
      process_start(&process_18, NULL);
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
  event_data_ready_8 = process_alloc_event();

  init_comms(&unicast_callbacks);

  // schedule jobs in scheduler


  PROCESS_END();
}

PROCESS_THREAD(process_8, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x140,x158 = 0;
  x158 = (int) readTemperature();
  char result[5];
  itoa(x158, result, 10);
  char *message = encode(8, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_11, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x147,x165 = 0;
  x165 = (int) readTemperature();
  char result[5];
  itoa(x165, result, 10);
  char *message = encode(11, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_12, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x146,x164 = 0;
  x164 = (int) readTemperature();
  char result[5];
  itoa(x164, result, 10);
  char *message = encode(12, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_15, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x148,x166 = 0;
  x166 = (int) readTemperature();
  char result[5];
  itoa(x166, result, 10);
  char *message = encode(15, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}

PROCESS_THREAD(process_18, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x149,x167 = 0;
  x167 = (int) readTemperature();
  char result[5];
  itoa(x167, result, 10);
  char *message = encode(18, result);
  send_message(node1, message);


  printf("\n");
  END:break;
  PROCESS_END();
}
