#include <linux/clocksource.h>
#include <linux/clockchips.h>
#include <linux/interrupt.h>
#include <linux/irq.h>

#include <asm/irq.h>
#include <asm/csr.h>
#include <asm/sbi.h>

static DEFINE_PER_CPU(struct clock_event_device, clock_event);

static int riscv_timer_set_next_event(unsigned long delta,
	struct clock_event_device *evdev)
{
	unsigned long new_timecmp;

	/* Set comparator */
	new_timecmp = csr_read(stime) + delta;
	csr_write(stimecmp, new_timecmp);

	/* Timer overflowed if high bit of subtraction is clear */
	return (int32_t)(csr_read(stime) - new_timecmp) >= 0 ? -ETIME : 0;
}

static void riscv_timer_set_mode(enum clock_event_mode mode,
	struct clock_event_device *evdev)
{
	switch (mode) {
	case CLOCK_EVT_MODE_ONESHOT:
	case CLOCK_EVT_MODE_UNUSED:
	case CLOCK_EVT_MODE_SHUTDOWN:
	case CLOCK_EVT_MODE_RESUME:
		break;
	case CLOCK_EVT_MODE_PERIODIC:
	default:
		BUG();
	}
}

irqreturn_t timer_interrupt(int irq, void *dev_id)
{
	int cpu = smp_processor_id();
	struct clock_event_device *evdev = &per_cpu(clock_event, cpu);
	evdev->event_handler(evdev);
	return IRQ_HANDLED;
}

static struct irqaction timer_irq = {
	.handler = timer_interrupt,
	.flags = IRQF_DISABLED | IRQF_TIMER,
	.name = "timer",
};


static cycle_t riscv_rdtime(struct clocksource *cs)
{
	return csr_read(stime);
}

static struct clocksource riscv_clocksource = {
	.name = "riscv_clocksource",
	.rating = 300,
	.read = riscv_rdtime,
#ifdef CONFIG_64BITS
	.mask = CLOCKSOURCE_MASK(64),
#else
	.mask = CLOCKSOURCE_MASK(32),
#endif /* CONFIG_64BITS */
	.flags = CLOCK_SOURCE_IS_CONTINUOUS,
};

void __init init_clockevent(void)
{
	int cpu = smp_processor_id();
	struct clock_event_device *ce = &per_cpu(clock_event, cpu);

	*ce = (struct clock_event_device){
		.name = "riscv_timer_clockevent",
		.features = CLOCK_EVT_FEAT_ONESHOT,
		.rating = 300,
		.cpumask = cpumask_of(cpu),
		.set_mode = riscv_timer_set_mode,
		.set_next_event = riscv_timer_set_next_event,
	};

	clockevents_config_and_register(ce, sbi_timebase(), 100, 0x7fffffff);
}

void __init time_init(void)
{
	clocksource_register_hz(&riscv_clocksource, sbi_timebase());
	setup_irq(IRQ_TIMER, &timer_irq);
	init_clockevent();
}
