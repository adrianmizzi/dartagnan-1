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
PROCESS(process_1, "Process 1");
AUTOSTART_PROCESSES(&init_process, &scheduler_process);
/*---------------------------------------------------------------------------*/

static process_event_t event_data_ready_2;
static process_event_t event_data_ready_3;
static process_event_t event_data_ready_4;
static process_event_t event_data_ready_5;
static process_event_t event_data_ready_6;
static process_event_t event_data_ready_7;
static process_event_t event_data_ready_8;
static process_event_t event_data_ready_9;
static process_event_t event_data_ready_10;
static process_event_t event_data_ready_11;
static process_event_t event_data_ready_12;
static process_event_t event_data_ready_13;
static process_event_t event_data_ready_14;
static process_event_t event_data_ready_15;
static process_event_t event_data_ready_16;
static process_event_t event_data_ready_17;
static process_event_t event_data_ready_18;
static process_event_t event_data_ready_19;


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
      process_post(&process_1, event_data_ready_2, incoming);
      break;
    case 3 :
      process_post(&process_1, event_data_ready_3, incoming);
      break;
    case 4 :
      process_post(&process_1, event_data_ready_4, incoming);
      break;
    case 5 :
      process_post(&process_1, event_data_ready_5, incoming);
      break;
    case 6 :
      process_post(&process_1, event_data_ready_6, incoming);
      break;
    case 7 :
      process_post(&process_1, event_data_ready_7, incoming);
      break;
    case 8 :
      process_post(&process_1, event_data_ready_8, incoming);
      break;
    case 9 :
      process_post(&process_1, event_data_ready_9, incoming);
      break;
    case 10 :
      process_post(&process_1, event_data_ready_10, incoming);
      break;
    case 11 :
      process_post(&process_1, event_data_ready_11, incoming);
      break;
    case 12 :
      process_post(&process_1, event_data_ready_12, incoming);
      break;
    case 13 :
      process_post(&process_1, event_data_ready_13, incoming);
      break;
    case 14 :
      process_post(&process_1, event_data_ready_14, incoming);
      break;
    case 15 :
      process_post(&process_1, event_data_ready_15, incoming);
      break;
    case 16 :
      process_post(&process_1, event_data_ready_16, incoming);
      break;
    case 17 :
      process_post(&process_1, event_data_ready_17, incoming);
      break;
    case 18 :
      process_post(&process_1, event_data_ready_18, incoming);
      break;
    case 19 :
      process_post(&process_1, event_data_ready_19, incoming);
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
  event_data_ready_3 = process_alloc_event();
  event_data_ready_4 = process_alloc_event();
  event_data_ready_5 = process_alloc_event();
  event_data_ready_6 = process_alloc_event();
  event_data_ready_7 = process_alloc_event();
  event_data_ready_8 = process_alloc_event();
  event_data_ready_9 = process_alloc_event();
  event_data_ready_10 = process_alloc_event();
  event_data_ready_11 = process_alloc_event();
  event_data_ready_12 = process_alloc_event();
  event_data_ready_13 = process_alloc_event();
  event_data_ready_14 = process_alloc_event();
  event_data_ready_15 = process_alloc_event();
  event_data_ready_16 = process_alloc_event();
  event_data_ready_17 = process_alloc_event();
  event_data_ready_18 = process_alloc_event();
  event_data_ready_19 = process_alloc_event();

  init_comms(&unicast_callbacks);

  // schedule jobs in scheduler

  static struct etimer et;

  while (1) {
    // start main process every 60 seconds
    etimer_set(&et, 60*CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&et));
    printf ("\n");

    process_start(&process_1, NULL); 
  }

  PROCESS_END();
}

PROCESS_THREAD(process_1, ev, data)
{
  PROCESS_BEGIN();
  static struct etimer et;
  static int x8,x99,x100,x11,x15,x12,x14,x13,x10,x3,x7,x4,x6,x5,x21,x101,x102,x28,x23,x27,x24,x26,x25,x19,x103,x20,x104,x105,x106,x38,x107,x108,x41,x43,x42,x40,x31,x37,x32,x36,x33,x35,x34,x109,x49,x110,x111,x54,x51,x53,x52,x46,x112,x47,x113,x48,x114,x115,x116,x62,x117,x118,x65,x67,x66,x64,x57,x61,x58,x60,x59,x119,x72,x120,x121,x77,x74,x76,x75,x70,x122,x71,x123,x124,x125,x83,x126,x127,x86,x88,x87,x85,x80,x82,x81,x128,x92,x129,x130,x97,x94,x96,x95,x91,x131,x132,x133 = 0;
  static bool x1,x16,x9,x17,x18,x29,x22,x30,x44,x39,x45,x55,x50,x56,x68,x63,x69,x78,x73,x79,x89,x84,x90,x98,x93 = false;
  static bool x134[10];
  static bool x2[10];
  
  char *message4 = encode(4, "");
  send_message(node3, message4);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_4 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_4) {
    x5 = getReading(data);
  }

  x6 = (int) readTemperature();
  
  char *message3 = encode(3, "");
  send_message(node2, message3);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_3 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_3) {
    x7 = getReading(data);
  }

  x10 = 50;
  x13 = (int) readTemperature();
  
  char *message2 = encode(2, "");
  send_message(node2, message2);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_2 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_2) {
    x14 = getReading(data);
  }

  x15 = 2;
  x17 = false;
  x25 = (int) readTemperature();
  
  char *message5 = encode(5, "");
  send_message(node2, message5);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_5 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_5) {
    x26 = getReading(data);
  }

  x27 = 2;
  x28 = 25;
  x34 = (int) readTemperature();
  
  char *message9 = encode(9, "");
  send_message(node2, message9);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_9 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_9) {
    x35 = getReading(data);
  }

  
  char *message8 = encode(8, "");
  send_message(node4, message8);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_8 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_8) {
    x36 = getReading(data);
  }

  
  char *message7 = encode(7, "");
  send_message(node3, message7);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_7 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_7) {
    x37 = getReading(data);
  }

  x40 = 50;
  
  char *message6 = encode(6, "");
  send_message(node3, message6);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_6 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_6) {
    x42 = getReading(data);
  }

  x43 = 1;
  
  char *message10 = encode(10, "");
  send_message(node3, message10);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_10 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_10) {
    x52 = getReading(data);
  }

  x53 = 1;
  x54 = 25;
  
  char *message14 = encode(14, "");
  send_message(node3, message14);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_14 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_14) {
    x59 = getReading(data);
  }

  
  char *message13 = encode(13, "");
  send_message(node5, message13);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_13 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_13) {
    x60 = getReading(data);
  }

  
  char *message12 = encode(12, "");
  send_message(node4, message12);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_12 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_12) {
    x61 = getReading(data);
  }

  x64 = 50;
  
  char *message11 = encode(11, "");
  send_message(node4, message11);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_11 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_11) {
    x66 = getReading(data);
  }

  x67 = 1;
  
  char *message15 = encode(15, "");
  send_message(node4, message15);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_15 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_15) {
    x75 = getReading(data);
  }

  x76 = 1;
  x77 = 25;
  
  char *message18 = encode(18, "");
  send_message(node4, message18);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_18 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_18) {
    x81 = getReading(data);
  }

  
  char *message17 = encode(17, "");
  send_message(node5, message17);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_17 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_17) {
    x82 = getReading(data);
  }

  x85 = 50;
  
  char *message16 = encode(16, "");
  send_message(node5, message16);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_16 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_16) {
    x87 = getReading(data);
  }

  x88 = 1;
  
  char *message19 = encode(19, "");
  send_message(node5, message19);
  // wait for max 10 seconds
  etimer_set(&et, 10*CLOCK_SECOND);
  while (ev != event_data_ready_19 && !etimer_expired(&et)){
    PROCESS_YIELD();
  }

  if (etimer_expired(&et)) {
    printf ("No reading received in this call.\n");
    goto END;
  } else if (ev == event_data_ready_19) {
    x95 = getReading(data);
  }

  x96 = 1;
  x97 = 25;
  x4 = x5 || x6;
  x12 = x13 + x14;
  x100 = x10;
  x105 = x5;
  x104 = x6;
  x103 = x7;
  x24 = x25 + x26;
  x101 = x28;
  x106 = x17;
  x33 = x34 || x35;
  x41 = x42 + x43;
  x108 = x40;
  x109 = x17;
  x115 = x34;
  x114 = x35;
  x113 = x36;
  x112 = x37;
  x51 = x52 + x53;
  x110 = x54;
  x116 = x17;
  x58 = x59 || x60;
  x65 = x66 + x67;
  x118 = x64;
  x119 = x17;
  x124 = x59;
  x123 = x60;
  x122 = x61;
  x74 = x75 + x76;
  x120 = x77;
  x125 = x17;
  x80 = x81 || x82;
  x86 = x87 + x88;
  x127 = x85;
  x128 = x17;
  x132 = x81;
  x131 = x82;
  x94 = x95 + x96;
  x129 = x97;
  x133 = x17;
  x3 = x4 || x7;
  x11 = x12 + x15;
  x20 = x105 || x104;
  x23 = x24 + x27;
  x32 = x33 || x36;
  x39 = x40 > x41;
  x107 = x41;
  x48 = x115 || x114;
  x50 = x51 > x54;
  x111 = x51;
  x57 = x58 || x61;
  x63 = x64 > x65;
  x117 = x65;
  x71 = x124 || x123;
  x73 = x74 > x77;
  x121 = x74;
  x84 = x85 > x86;
  x126 = x86;
  x91 = x132 || x131;
  x93 = x94 > x97;
  x130 = x94;
  x9 = x10 > x11;
  x99 = x11;
  x19 = x20 || x103;
  x22 = x23 > x28;
  x102 = x23;
  x31 = x32 || x37;
  x44 = x108 == x107;
  x47 = x48 || x113;
  x55 = x111 == x110;
  x68 = x118 == x117;
  x70 = x71 || x122;
  x78 = x121 == x120;
  x89 = x127 == x126;
  x98 = x130 == x129;
  x16 = x100 == x99;
  x29 = x102 == x101;
  x38 = x39 || x44;
  x46 = x47 || x112;
  x49 = x50 || x55;
  x62 = x63 || x68;
  x72 = x73 || x78;
  x83 = x84 || x89;
  x92 = x93 || x98;
  x8 = x9 || x16;
  x21 = x22 || x29;
  if (x31)
    x30=x38;
  else
    x30=x109;
  if (x46)
    x45=x49;
  else
    x45=x116;
  if (x57)
    x56=x62;
  else
    x56=x119;
  if (x70)
    x69=x72;
  else
    x69=x125;
  if (x80)
    x79=x83;
  else
    x79=x128;
  if (x91)
    x90=x92;
  else
    x90=x133;
  if (x3)
    x1=x8;
  else
    x1=x17;
  if (x19)
    x18=x21;
  else
    x18=x106;
  x2[0] = x1;
  x2[1] = x18;
  x2[2] = x30;
  x2[3] = x45;
  x2[4] = x56;
  x2[5] = x69;
  x2[6] = x79;
  x2[7] = x90;
  
  printBoolArray(x2);

  printf("\n");
  END:break;
  PROCESS_END();
}
